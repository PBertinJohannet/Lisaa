use scanner::Scanner;
use parser::Parser;

pub struct Ytp {
    source: String,
}
impl Ytp {
    pub fn new(source: String) -> Self {
        Ytp { source: source }
    }

    pub fn run(&mut self) -> Result<(), String> {
        //println!("source : {}", self.source);

        let tokens = match Scanner::new(&self.source).tokens() {
            Ok(tokens) => tokens,
            Err(e) => return Err(e),
        };

        let tree = Parser::new(tokens).parse();
        match tree {
            Ok(e) => eprintln!("parsing completed"),
            Err(e) => e.iter().map(|p_err|eprintln!("{}\n", p_err)).collect(),
        }
        Ok(())
    }
}
