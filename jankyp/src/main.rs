use clap::Clap;
mod parse_pretty_parse;

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Pretty(Pretty),
}

/// Parse, pretty-print, and re-parse a JavaScript file, and check that the two
/// ASTs are identical.
#[derive(Clap)]
struct Pretty {
    /// JavaScript file
    #[clap()]
    filename: String,
}

fn main() {
    let opts = Opts::parse();
    match opts.subcmd {
        SubCommand::Pretty(pretty) => parse_pretty_parse::main(pretty.filename),
    }
}
