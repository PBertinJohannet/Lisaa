//! Used to run a script and get its output in a buffer.
use lisaa::Lisaa;
use std::io::Cursor;

/// Maybe just run all the scripts in the scripts dir starting by test
/// This allows to run a script and create
/// Be carefull to not have different tests using the same file names.
/// Just call the main with the name of the test
pub struct Script {
    run: String,
}

impl Script {
    /// Creates a new script to run
    pub fn new(main: &str) -> Self {
        Script {
            run: main.to_string(),
        }
    }
    /// Runs the program and returns its output.
    pub fn run_program(&mut self, output_size: usize) -> Result<String, String> {
        let mut output = ['a'].iter().cycle().take(output_size).collect::<String>();
        {
            let mut output_stream = Cursor::new(unsafe { output.as_bytes_mut() });
            Lisaa::new(self.run.clone(), &mut output_stream, true).run()?;
        }
        Ok(output)
    }
}
