use expression::LiteralExpr;
use statement::TypedVar;

#[derive(Debug)]
pub struct NativeFunc {
    name: String,
    args: Vec<TypedVar>,
}
impl NativeFunc {
    pub fn print() -> Self {
        NativeFunc {
            name: "print".to_string(),
            args: vec![TypedVar::any()],
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn args(&self) -> Vec<TypedVar> {
        self.args.clone()
    }
}

pub fn get_native(
    library: &str,
) -> Vec<(
    String,
    Box<Fn(Vec<LiteralExpr>) -> Result<LiteralExpr, String>>,
)> {
    match library {
        "base" => vec![("print".to_string(), Box::new(print))], //, time, rand],
        _ => vec![],
    }
}
pub fn get_native_types(library: &str) -> Vec<NativeFunc> {
    match library {
        "base" => vec![NativeFunc::print()], //, time, rand],
        _ => vec![],
    }
}

pub fn print(args: Vec<LiteralExpr>) -> Result<LiteralExpr, String> {
    for i in args {
        print!("{}", i);
    }
    println!("");
    Ok(LiteralExpr::NUMBER(0.0))
}
