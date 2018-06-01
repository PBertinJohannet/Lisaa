use statement::{LisaaType, TypedVar};

#[derive(Debug)]
pub struct NativeFunc {
    name: String,
    args: Vec<TypedVar>,
    ret: LisaaType,
}
impl NativeFunc {
    pub fn print() -> Self {
        NativeFunc {
            name: "print".to_string(),
            args: vec![TypedVar::new(LisaaType::Num, "a".to_string())],
            ret: LisaaType::Void,
        }
    }
    pub fn rand() -> Self {
        NativeFunc {
            name: "rand".to_string(),
            args: vec![],
            ret: LisaaType::Num,
        }
    }
    pub fn newslice() -> Self {
        NativeFunc {
            name: "newslice".to_string(),
            args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
            ret: LisaaType::slice(LisaaType::Any),
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn args(&self) -> Vec<TypedVar> {
        self.args.clone()
    }
    pub fn ret(&self) -> &LisaaType {
        &self.ret
    }
}

pub fn get_native_types(library: &str) -> Vec<NativeFunc> {
    match library {
        "base" => vec![
            NativeFunc::print(),
            NativeFunc::rand(),
            NativeFunc::newslice(),
        ], //, time, rand],
        _ => vec![],
    }
}
