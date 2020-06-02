use libjankscripten::javascript::*;
use std::fmt::Display;
use std::fs;
use std::path::{Path, PathBuf};

fn add_pretty_prefix_to_filename(filename: &Path) -> PathBuf {
    let pretty_name = format!("pretty-{}", filename.file_name().unwrap().to_str().unwrap());
    return filename.with_file_name(pretty_name);
}

pub fn main<P>(filename: P)
where
    P: AsRef<Path> + Display,
{
    let filename = filename.as_ref();
    let js_code = fs::read_to_string(filename).unwrap();
    match parse(&js_code) {
        Err(err) => {
            eprintln!("Could not parse {}\n{}", filename.display(), err);
            std::process::exit(1);
        }
        Ok(ast) => {
            let new_code = ast.to_pretty(120);
            match parse(&new_code) {
                Err(err) => {
                    let new_filename = add_pretty_prefix_to_filename(&filename);
                    eprintln!(
                        "Could not re-parse. See {}.\n{}",
                        new_filename.display(),
                        err
                    );
                    fs::write(new_filename, new_code).expect("writing file");
                    std::process::exit(1);
                }
                Ok(new_ast) => {
                    if ast != new_ast {
                        let new_filename = add_pretty_prefix_to_filename(&filename);
                        eprintln!(
                            "Re-parsed AST is not identical to the original AST. See {}.",
                            new_filename.display()
                        );
                        fs::write(new_filename, new_code).expect("writing file");
                        std::process::exit(1);
                    }
                }
            }
        }
    }
}
