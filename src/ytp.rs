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

        let tokens = Scanner::new(&self.source).tokens()?;
        let tree = Parser::new(tokens).program();

        match tree {
            Ok(e) => {
                let mut inter = Interpreter::new(None);
                inter.run(e);
                println!("{:?}", inter.state());
            },
            Err(e) => e.iter().map(|p_err| eprintln!("{}\n", p_err)).collect(),
        }
        Ok(())
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_expr(){
        let source = "a= 1;
        b = 2+2;
        c = \"h\";
        a = b;
        b = a*2+55-12;".to_string();
        assert_eq!(Ytp::new(source).run().is_ok(), true);
    }
    #[test]
    fn test_scope(){
        let source = "a= 1;
        {
            b = 1;
            a = b+1;
        }
        {
            a = 2;
        }
        c = a+2;";
        assert_eq!(Ytp::new(source.to_string()).run().is_ok(), true);
    }
}
