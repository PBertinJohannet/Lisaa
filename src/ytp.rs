//! The ytp module, where the interpreter just calls the other modules.
use scanner::Scanner;
use parser::Parser;
use interpreter::Interpreter;
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

        let tokens = match Scanner::new(&self.source).tokens() {
            Ok(tokens) => tokens,
            Err(e) => return Err(e),
        };

        let tree = Parser::new(tokens).parse();
        match tree {
            Ok(e) => {
                let mut inter = Interpreter::new();
                e.iter().map(|expr| match inter.evaluate(expr) {
                    Ok(res) => println!("{}", res),
                    Err(s) => eprintln!("{}", s),
                }).collect()
            },
            Err(e) => e.iter().map(|p_err| eprintln!("{}\n", p_err)).collect(),
        }
        Ok(())
    }
}
