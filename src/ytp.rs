//! The ytp module, where the interpreter just calls the other modules.
use scanner::Scanner;
use parser::Parser;
use interpreter::Interpreter;
use typecheck::TypeChecker;
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
        let mut inter = Interpreter::new(None);
        inter.run(tree)?;
        Ok(())
    }
}


