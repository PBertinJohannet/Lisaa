#![deny(missing_docs)]

//! An interpreter for a language,
//! This is an experiment.
//!

extern crate clap;
#[macro_use]
extern crate lazy_static;


pub mod parser;
pub mod expression;
pub mod ytp;
pub mod keywords;
pub mod token;
pub mod scanner;
pub mod interpreter;
pub mod operations;
pub mod statement;

#[cfg(test)]
mod tests {
    // tests coming afterwards.
}
