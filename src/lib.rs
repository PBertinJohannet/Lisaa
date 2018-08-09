#![deny(missing_docs)]
#![feature(box_patterns)]
#![feature(int_to_from_bytes)]
//! An interpreter for a language,
//! This is an experiment.
//!

extern crate rand;

extern crate time;
#[macro_use]
extern crate lazy_static;

mod compile;
//mod compile_req;
mod expression;
mod generic_inference;
mod keywords;
pub mod lisaa;
mod monomorphise;
mod native;
mod parser;
mod scanner;
pub mod script;
mod statement;
mod token;
mod typecheck;
mod types;
mod vm;
