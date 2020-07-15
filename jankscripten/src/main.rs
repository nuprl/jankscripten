use clap::Clap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

#[derive(Clap)]
struct Compile {
    #[clap(short, long)]
    output: Option<String>,
    input: String,
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

fn compile_notwasm(input: &str, output: &Path) {
    use libjankscripten::notwasm;
    let parsed = notwasm::parse(input);
    let wasm = notwasm::compile(parsed).expect("compile error");
    fs::write(output, wasm).expect("writing file");
}

fn compile(opts: Compile) {
    let input_path = Path::new(&opts.input);
    let ext = expect_extension(input_path);
    match ext {
        "notwasm" => {
            let output_path = make_output_filename(&opts.output, input_path, "wasm");
            let input = read_file(input_path);
            compile_notwasm(&input, output_path.as_path());
        }
        _ => {
            eprintln!("Unsupported extension: .{}", ext);
            process::exit(1);
        }
    }
}

fn parse(opts: Parse) {
    let input_path = Path::new(&opts.input);
    let ext = expect_extension(input_path);
    match ext {
        "js" => {
            let input = read_file(input_path);
            match libjankscripten::javascript::parse(&input) {
                Ok(stmt) => {
                    libjankscripten::jankierscript::from_javascript::stmt(stmt);
                    println!("successfully translated {} to jankierscript", opts.input);
                }
                Err(err) => {
                    eprintln!("{}:\n{}", opts.input, err);
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("Unsupported extension: .{}", ext);
            process::exit(1);
        }
    }
}

fn main() {
    let opts = Opts::parse();
    match opts.subcmd {
        SubCommand::Compile(opts) => compile(opts),
        SubCommand::Parse(opts) => parse(opts),
    }
}
