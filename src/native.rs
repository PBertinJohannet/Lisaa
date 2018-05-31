use expression::LiteralExpr;
use statement::TypedVar;
use vm::OP;

#[derive(Debug)]
pub struct NativeFunc {
    name: String,
    args: Vec<TypedVar>,
    ret: String,
}
impl NativeFunc {
    pub fn print() -> Self {
        NativeFunc {
            name: "print".to_string(),
            args: vec![TypedVar::new("num".to_string(), "a".to_string())],
            ret: "".to_string()
        }
    }
    pub fn rand() -> Self {
        NativeFunc {
            name: "rand".to_string(),
            args: vec![],
            ret : "num".to_string(),
        }
    }
    pub fn newslice() -> Self {
        NativeFunc {
            name: "newslice".to_string(),
            args: vec![TypedVar::new("num".to_string(), "n".to_string())],
            ret : "slice".to_string(),
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn args(&self) -> Vec<TypedVar> {
        self.args.clone()
    }
    pub fn ret(&self) -> String{
        self.ret.to_string()
    }
}

pub fn get_native_types(library: &str) -> Vec<NativeFunc> {
    match library {
        "base" => vec![NativeFunc::print(),
                       NativeFunc::rand(),
                       NativeFunc::newslice()], //, time, rand],
        _ => vec![],
    }
}


