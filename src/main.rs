#![feature(box_patterns)]
#![feature(int_to_from_bytes)]
#![deny(missing_docs)]
#![deny(unused_must_use)]
//! An interpreter for a language,
//! This is an experiment.
//!
extern crate clap;
extern crate rand;
#[macro_use]
extern crate lazy_static;
extern crate time;
mod compile;
//mod compile_req;
mod expression;
mod generic_inference;
mod keywords;
mod lisaa;
mod monomorphise;
mod native;
mod parser;
mod scanner;
pub mod statement;
mod token;
mod typecheck;
mod types;
mod vm;

#[allow(unused_imports)]
use std::io::{self, Read};

use clap::{App, Arg};
use lisaa::Lisaa;
#[allow(unused_imports)]
use std::fs::File;
use std::process::exit;
fn main() {
    let matches = App::new("Lisaa")
        .version("0.0.0")
        .author("Pierre Bertin-Johannet")
        .about("Interpreter for the Lisaa lang")
        .arg(
            Arg::with_name("INPUT")
                .index(1)
                .help("the file to run")
                .default_value("leak.lisaa")
                .required(true),
        )
        .arg(
            Arg::with_name("VERBOSE")
                .index(2)
                .help("print generated bytecode ")
                .default_value("0")
                .required(true),
        )
        .get_matches();

    let input_file = matches.value_of("INPUT").unwrap();
    let verbose = matches.value_of("VERBOSE").unwrap();
    let mut stdout = io::stdout();
    exit(
        match Lisaa::new(input_file.to_owned(), &mut stdout, verbose == "1").run() {
            Ok(_) => 0,
            Err(err) => {
                eprintln!("error: {:?}", err);
                1
            }
        },
    );
}
