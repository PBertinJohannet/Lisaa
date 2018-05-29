use expression::LiteralExpr;
use statement::TypedVar;
use vm::OP;

#[derive(Debug)]
pub struct NativeFunc {
    name: String,
    args: Vec<TypedVar>,
}
impl NativeFunc {
    pub fn print() -> Self {
        NativeFunc {
            name: "print".to_string(),
            args: vec![TypedVar::new("num".to_string(), "a".to_string())],
        }
    }
    pub fn rand() -> Self {
        NativeFunc {
            name: "rand".to_string(),
            args: vec![],
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn args(&self) -> Vec<TypedVar> {
        self.args.clone()
    }
}

pub fn get_native_types(library: &str) -> Vec<NativeFunc> {
    match library {
        "base" => vec![NativeFunc::print()], //, time, rand],
        _ => vec![],
    }
}

pub fn get_compiled_native(library: &str) -> Vec<(String, Vec<OP>)>{
    vec![("print".to_string(), vec![OP::PrintNum]),
         ("rand".to_string(), vec![OP::RandNum])
        ]
}

