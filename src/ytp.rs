//! The ytp module, where the interpreter just calls the other modules.
use compile::Compiler;
//use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
use statement::{FunctionDecl, Program};
use std::collections::HashMap;
use time::PreciseTime;
use typecheck::TypeChecker;
use vm::Vm;

/// The interpreter, contains the code.
pub struct Ytp {
    source: String,
}
impl Ytp {
    /// Creates a new instance of the interpreter with the given source
    pub fn new(source: String) -> Self {
        Ytp { source: source }
    }
    /// Runs this instance of the interpreter.
    /// Will parse then interpret.
    pub fn run(&mut self) -> Result<(), String> {
        //println!("source : {}", self.source);

        let tokens = Scanner::new(&self.source).tokens()?;
        let mut tree = Parser::new(tokens).program().map_err(|e| {
            for p_err in e.iter() {
                eprintln!("{}\n", p_err);
            }
            String::from("Compilation aborted because of preceding errors.")
        })?;
        tree.initiate_methods();
        if let Err(e) = TypeChecker::new().resolve(&mut tree) {
            println!("TypeError : {}", e);
            return Err(String::from(
                "Compilation aborted because of preceding errors.",
            ));
        }

        self.do_vm(tree.clone())?;

        Ok(())
    }

    pub fn do_vm(&self, tree: Program) -> Result<(), String> {
        let code = Compiler::new()
            .compile(&tree)
            .map_err(|e| format!("compilation error : {:?}", e))?;
        for c in code.iter() {
            println!("{:?}", c);
        }

        let mut vm = Vm::new();
        let start = PreciseTime::now();
        vm.run(code);
        let end = PreciseTime::now();
        let diff = start.to(end).num_milliseconds();

        println!("vm state : {:?}", vm);

        println!("vm time : {:?}ms", diff);
        Ok(())
    }
}
