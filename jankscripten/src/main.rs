use clap::Clap;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

#[derive(Clap)]
struct Compile {
    #[clap(short, long)]
    output: Option<String>,
    input: String,

    #[clap(short, long)]
    jankyscript_dump: bool,

    #[clap(short, long)]
    notwasm_dump: bool,
    /// Only for debugging.
    #[clap(long)]
    disable_gc: bool,
    #[clap(long)]
    stdlib: Option<String>,
    /// Dump the offsets of interned strings to the console (for debugging).
    #[clap(long)]
    dump_interned: bool,
}

#[derive(Clap)]
struct Parse {
    input: String,
}

#[derive(Clap)]
enum SubCommand {
    Compile(Compile),
    Parse(Parse),
}

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

impl Compile {
    fn libjankscripten_opts(&self) -> libjankscripten::opts::Opts {
        let mut compile_opts = libjankscripten::opts::Opts::new();
        if self.disable_gc {
            compile_opts.disable_gc = true;
        }
        if let Some(p) = &self.stdlib {
            let stdlib_source_code = fs::read_to_string(p).expect(&format!("reading {}", p));
            compile_opts.notwasm_stdlib_source_code = Some(stdlib_source_code);
        }
        compile_opts
    }
}

fn display_interned_strings_offset(interned_strings: HashMap<String, u32>) {
    let mut inverted = interned_strings
        .into_iter()
        .map(|(k, v)| (v, k))
        .collect::<Vec<(u32, String)>>();
    inverted.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
    for (offset, s) in inverted {
        println!("{} -> {}", offset, s);
    }
}

fn make_output_filename(
    opt_output: &Option<String>,
    input_path: &Path,
    default_extension: &str,
) -> PathBuf {
    match opt_output {
        Some(name) => PathBuf::from(name),
        None => input_path.with_extension(default_extension),
    }
}

fn read_file(path: &Path) -> String {
    match fs::read_to_string(path) {
        Err(err) => {
            eprintln!("Error reading from {}\n{}", path.to_string_lossy(), err);
            process::exit(1);
        }
        Ok(s) => s,
    }
}

fn expect_extension(p: &Path) -> &str {
    match p.extension() {
        None => {
            eprintln!("Input filename does not have an extension.");
            process::exit(1);
        }
        Some(ext) => ext.to_str().expect("filename extension is not UTF-8"),
    }
}

fn compile_notwasm(opts: Compile, input: &str, output: &Path) {
    use libjankscripten::notwasm;
    let parsed = notwasm::parse(opts.input.as_str(), input);

    let wasm = match notwasm::compile(&mut opts.libjankscripten_opts(), parsed, |_| ()) {
        Ok((wasm, interned_strings)) => {
            if opts.dump_interned {
                display_interned_strings_offset(interned_strings);
            }
            wasm
        }
        Err(e) => panic!("{}", e),
    };
    fs::write(output, wasm).expect("writing file");
}

fn compile(opts: Compile) {
    let input_path = Path::new(&opts.input);
    let ext = expect_extension(input_path);
    match ext {
        "notwasm" => {
            let output_path = make_output_filename(&opts.output, input_path, "wasm");
            let input = read_file(input_path);
            compile_notwasm(opts, &input, output_path.as_path());
        }
        "js" => {
            let js_code = read_file(input_path);
            let (wasm_bin, interned_strings) = libjankscripten::javascript_to_wasm(
                opts.libjankscripten_opts(),
                &opts.input,
                &js_code,
                |janky| {
                    if opts.jankyscript_dump {
                        eprintln!("{}", janky);
                    }
                },
                |notwasm| {
                    if opts.notwasm_dump {
                        eprintln!("{}", notwasm);
                    }
                },
            )
            .expect("compile error");
            if opts.dump_interned {
                display_interned_strings_offset(interned_strings);
            }
            let output_path = make_output_filename(&opts.output, input_path, "wasm");
            fs::write(output_path, wasm_bin).expect("writing wasm output");
        }
        _ => {
            eprintln!("Unsupported extension: .{}", ext);
            process::exit(1);
        }
    }
}

fn read_javascript(raw_path: &String) -> String {
    let input_path = Path::new(raw_path);
    let ext = expect_extension(input_path);
    match ext {
        "js" => read_file(input_path),
        _ => {
            eprintln!("Unsupported extension: .{}", ext);
            process::exit(1);
        }
    }
}

fn parse_javascript(
    src_name: &str,
    input: &String,
    input_path: &String,
) -> libjankscripten::javascript::Stmt {
    match libjankscripten::javascript::parse(src_name, &input) {
        Ok(stmt) => stmt,
        Err(err) => {
            eprintln!("{}:\n{}", input_path, err);
            process::exit(1);
        }
    }
}

fn desugar_javascript(
    stmt: &mut libjankscripten::javascript::Stmt,
) -> &libjankscripten::javascript::Stmt {
    let mut name_gen = libjankscripten::javascript::NameGen::default();
    libjankscripten::javascript::desugar(stmt, &mut name_gen);
    stmt
}

fn parse(opts: Parse) {
    ///// Source Code -> JavaScript
    let src_javascript = read_javascript(&opts.input);
    let mut parsed_javascript = parse_javascript(&opts.input, &src_javascript, &opts.input);
    // desugaring mutates the stmt in place
    desugar_javascript(&mut parsed_javascript);
    let desugared_javascript = parsed_javascript;

    ///// JavaScript -> JankierScript
    let _jankyscript = libjankscripten::jankierscript::from_javascript(desugared_javascript);
}

fn main() {
    let opts = Opts::parse();
    match opts.subcmd {
        SubCommand::Compile(opts) => compile(opts),
        SubCommand::Parse(opts) => parse(opts),
    }
}
