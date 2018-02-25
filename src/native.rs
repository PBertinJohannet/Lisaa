use expression::LiteralExpr;

pub fn get_native(library : &str) -> Vec<(String, Box<Fn(Vec<LiteralExpr>) -> Result<LiteralExpr, String>>)>{
    match library{
        "base" => vec![("print".to_string(), Box::new(print))],//, time, rand],
        _ => vec![]
    }
}

pub fn print(args : Vec<LiteralExpr>) -> Result<LiteralExpr, String> {
    for i in args {
        print!("{}", i);
    }
    println!("");
    Ok(LiteralExpr::NUMBER(0.0))
}