//! The ytp module, where the interpreter just calls the other modules.
use compile::Compiler;
//use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
use statement::{FunctionDecl, Program};
use std::collections::{HashMap, HashSet};
use time::PreciseTime;
use typecheck::TypeChecker;
use vm::Vm;
use std::fs::File;
use std::io::{Read, Write, self};


/// The interpreter, contains the code.
pub struct Lisaa<'a> {
    source: String,
    output_stream: &'a mut Write,
}
impl<'a> Lisaa<'a> {
    /// Creates a new instance of the interpreter with the given source
    pub fn new(source: String, output : &'a mut Write) -> Self {
        Lisaa { source: source, output_stream : output}
    }


    fn find_source(name : String) -> Result<String, String>{
        if File::open(name.to_owned()).is_ok(){
            Ok(name)
        } else if File::open(format!("{}.lisaa", name.to_owned())).is_ok(){
            Ok(format!("{}.lisaa", name.to_owned()))
        } else {
            Err(format!("cannot find source for {}", name))
        }
    }

    /// Finds the source associated with an import.
    fn open_source(source_name : String) -> Result<String , String> {
        let mut file = File::open(Lisaa::find_source(source_name.clone())?).unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).map_err({
                |e|format!(
                    "could not read file : {},\
                 error : {} ",
                    source_name, e
                )
            })?;
        Ok(contents)
    }

    fn parse(&self) -> Result<Program, String>{
        let mut to_import = vec![self.source.clone()];
        let mut imported = HashSet::new();
        let mut program = Program::empty();

        while let Some(file) = to_import.pop() {
            let tokens = Scanner::new(Lisaa::open_source(file)?).tokens()?;
            let (mut tree, imports) = Parser::new(tokens).program().map_err(|e| {
                for p_err in e.iter() {
                    eprintln!("{}\n", p_err);
                }
                String::from("Compilation aborted because of preceding errors.")
            })?;
            for imp in imports {
                if !imported.contains(&imp){
                    to_import.push(imp.clone());
                    imported.insert(imp);
                }
            }
            tree.initiate_methods();
            program.merge(tree)?;
        }
        Ok(program)
    }
    /// Runs this instance of the interpreter.
    /// Will parse then interpret.
    pub fn run(&mut self) -> Result<&mut Self, String> {
        //println!("source : {}", self.source);
        println!("\nlisaa : Running {}\n\n", self.source);

        let mut tree = self.parse()?;
        if let Err(e) = TypeChecker::new().resolve(&mut tree) {
            println!("TypeError : {}", e);
            return Err(String::from(
                "Compilation aborted because of preceding errors.",
            ));
        }

        self.do_vm(tree.clone())?;

        Ok(self)
    }

    fn do_vm(&mut self, tree: Program) -> Result<(), String> {
        let code = Compiler::new()
            .compile(&tree)
            .map_err(|e| format!("compilation error : {:?}", e))?;
        for c in code.iter() {
            println!("{:?}", c);
        }

        let mut vm = Vm::new(&mut self.output_stream);
        let start = PreciseTime::now();
        vm.run(code);
        let end = PreciseTime::now();
        let diff = start.to(end).num_milliseconds();

        //println!("vm state {:?}", vm);

        //println!("vm time : {:?}ms", diff);
        Ok(())
    }
}
