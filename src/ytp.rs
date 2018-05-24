//! The ytp module, where the interpreter just calls the other modules.
use scanner::Scanner;
use parser::Parser;
use interpreter::Interpreter;
use typecheck::TypeChecker;
use compile::Compiler;
use vm::Vm;
use time::PreciseTime;
use std::collections::HashMap;
use statement::FunctionDecl;
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
        let mut tree = Parser::new(tokens).program().map_err(|e|{
                for p_err in e.iter(){
                    eprintln!("{}\n", p_err);
                }
                String::from("Compilation aborted because of preceding errors.")
            })?;
        if let Err(e) = TypeChecker::new().resolve(&mut tree){
            println!("TypeError : {}", e);
            return Err(String::from("Compilation aborted because of preceding errors."));
        }

        self.do_vm(tree.clone())?;
        self.do_interpret(tree);
        self.do_rust();

        Ok(())
    }

    pub fn do_vm(&self, tree :  HashMap<String, FunctionDecl>) -> Result<(), String>{

        let code = Compiler::new().compile(&tree).map_err(|e|format!("compilation error : {:?}", e))?;
        //println!("code : {:?}", code);

        let mut vm = Vm::new();
        let start = PreciseTime::now();
        vm.run(code);
        let end = PreciseTime::now();
        let diff = start.to(end).num_milliseconds();


        println!("vm time : {:?}ms", diff);
        Ok(())
    }


    pub fn do_interpret(&self, tree : HashMap<String, FunctionDecl>){

        let mut inter = Interpreter::new(None);
        let start = PreciseTime::now();
        inter.run(tree);
        let end = PreciseTime::now();
        let diff = start.to(end).num_milliseconds();

        println!("interpreter time : {:?}ms", diff);

    }

    pub fn do_rust(&self){


        let start = PreciseTime::now();
        {
            let (to_find, mut a, mut b, mut found) = (73987, 0,0,false);
            while !found && a < to_find/2{
                a +=1;
                while !found && a < to_find/2{
                    b +=1;
                    if a*b==to_find{
                        found = true;
                    }
                }
            }
        }

        let end = PreciseTime::now();
        let diff = start.to(end).num_microseconds();

        println!("rust time : {:?}us", diff);
    }
}


