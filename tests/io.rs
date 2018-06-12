extern crate lisaa_lang;
use std::fs::File;
use std::io::prelude::*;
use std::io::Write;
use std::collections::HashMap;
use lisaa_lang::lisaa;
use std::io::Cursor;

struct Script {
    files : HashMap<String, String>,
    run : String,
}

impl Script {
    pub fn new() -> Self {
        Script {
            files : HashMap::new(),
            run : "main.lisaa".to_string(),
        }
    }

    pub fn with_file(&mut self, (name, content) : (&str, &str)) -> &mut Self{
        self.files.insert(name.to_owned(), content.to_owned());
        self
    }

    pub fn run(&mut self, name : &str, output_size : usize) -> Result<String, String>{
        for (name, content) in self.files.iter() {
            let mut file = File::create(name).unwrap_or_else(|e|panic!("cant create"));
            file.write(content.as_bytes());
        }
        let mut output = ['a'].iter().cycle().take(output_size).collect::<String>();
        {
            let mut output_stream = Cursor::new(unsafe {
                output.as_bytes_mut()
            });
            lisaa::Lisaa::new(name.to_owned(), &mut output_stream).run()?;
        }
        Ok(output)
    }
}


#[test]
fn test_hello_world() {
    let expected_output = String::from("hello, world");
    let main = "\
    import string\n\
    fn main(){\
        \"hello, world\".print();\
    }";
    //println!("main is : {}", main);
    let mut script = Script::new();
    let mut output_size = expected_output.len();
    script.with_file(("main.lisaa", main));
    assert_eq!(Ok(expected_output), script.run("main.lisaa", output_size));
}