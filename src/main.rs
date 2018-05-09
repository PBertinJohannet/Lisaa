#![deny(missing_docs)]

//! An interpreter for a language,
//! This is an experiment.
//!

extern crate clap;
#[macro_use]
extern crate lazy_static;

mod keywords;
mod token;
mod ytp;
mod scanner;
mod expression;
mod parser;
mod interpreter;
mod operations;
pub mod statement;
mod typecheck;
mod native;
use std::io::Read;

use clap::{Arg, App};
use std::fs::File;
use std::process::exit;
use ytp::Ytp;

fn main() {
    let matches = App::new("ytp")
        .version("0.0.0")
        .author("Pierre Bertin-Johannet")
        .about("Interpreter for the ytp lang")
        .arg(
            Arg::with_name("INPUT")
                .index(1)
                .help("the file to run")
                .required(true),
        )
        .get_matches();


    let input_file = matches.value_of("INPUT").unwrap();

    exit(match run_file(input_file) {
        Ok(_) => 0,
        Err(err) => {
            eprintln!("error: {:?}", err);
            1
        }
    });
}

/// Runs the given file with the interpreter.
fn run_file(input_file: &str) -> Result<(), String> {
    let mut file = match File::open(input_file) {
        Ok(file) => file,
        Err(e) => {
            return Err(
                format!(
                    "could not open file : {},\
                 error : {} ",
                    input_file,
                    e
                ).to_string(),
            );
        }
    };

    println!("\nytp : Running {}\n\n", input_file);



    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(file) => file,
        Err(e) => {
            return Err(
                format!(
                    "could not read file : {},\
             error : {} ",
                    input_file,
                    e
                ).to_string(),
            );
        }
    };
    return Ytp::new(contents).run();
}
