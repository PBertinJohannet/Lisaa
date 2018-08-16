use statement::{ClassDecl, FunctionDecl, FunctionSig, Statement, TraitDecl, TypeParam};
use std::collections::HashMap;
use types::{LisaaType, TypedVar};
use vm::OP;

/// A native function.
trait NativeFunc {
    fn print() -> FunctionDecl;
    fn empty() -> FunctionDecl;
    fn rand() -> FunctionDecl;
    fn num_funcs() -> Vec<FunctionDecl>;
    fn slice_funcs() -> Vec<FunctionDecl>;
}

impl NativeFunc for FunctionDecl {
    fn empty() -> Self {
        FunctionDecl::new_complete(
            None,
            true,
            "empty".to_owned(),
            vec![TypeParam::new("T".to_string(), "Any".to_string())],
            vec![],
            Statement::Native(vec![OP::PushNum(0.0)]),
            LisaaType::Class("T".to_string(), vec![]),
        )
    }
    fn print() -> Self {
        FunctionDecl::new_complete(
            None,
            true,
            "print".to_owned(),
            vec![],
            vec![TypedVar::new(LisaaType::Char, "a".to_owned())],
            Statement::Native(vec![OP::PrintChar, OP::PushNum(0.0)]),
            LisaaType::Void,
        )
    }
    fn rand() -> Self {
        FunctionDecl::new_complete(
            None,
            true,
            "rand".to_owned(),
            vec![],
            vec![],
            Statement::Native(vec![OP::RandNum]),
            LisaaType::Num,
        )
    }
    fn slice_funcs() -> Vec<Self> {
        vec![
            FunctionDecl::new_complete(
                None,
                true,
                "slice".to_owned(),
                vec![TypeParam::new("T".to_string(), "Any".to_string())],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::AllocSlice]),
                LisaaType::Class(
                    "slice".to_string(),
                    vec![LisaaType::Class("T".to_string(), vec![])],
                ),
            ),
            FunctionDecl::new_complete(
                Some(LisaaType::Class(
                    "slice".to_string(),
                    vec![LisaaType::Class("T".to_string(), vec![])],
                )),
                true,
                "slice::index".to_owned(),
                vec![TypeParam::new("T".to_string(), "Any".to_string())],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Add]),
                LisaaType::Class("T".to_string(), vec![]),
            ),
        ]
    }
    fn num_funcs() -> Vec<Self> {
        vec![
            FunctionDecl::new_complete(
                None,
                true,
                "num::toString".to_owned(),
                vec![],
                vec![],
                Statement::Native(vec![OP::ToStr]),
                LisaaType::Class("String".to_owned(), vec![]),
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::add".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Add]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::times".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Mul]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::modulo".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Swap2, OP::Mod]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::andand".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::AndAnd]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::oror".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::OrOr]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::or".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Or]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::minus".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Neg, OP::Add]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::divide".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Inv, OP::Mul]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::not".to_owned(),
                vec![],
                vec![],
                Statement::Native(vec![OP::Not]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::negate".to_owned(),
                vec![],
                vec![],
                Statement::Native(vec![OP::Neg]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::ne".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Eq, OP::Not]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::equals".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Eq]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::ge".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::LowerEq]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::le".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::GreaterEq]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::less".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::GreaterThan]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::greater".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::LowerThan]),
                LisaaType::Num,
            ),
            FunctionDecl::new_complete(
                None,
                true,
                "num::index".to_owned(),
                vec![],
                vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                Statement::Native(vec![OP::Add]),
                LisaaType::Num,
            ),
        ]
    }
}

pub fn get_native_funcs(library: &str) -> Vec<FunctionDecl> {
    match library {
        "base" => {
            let mut base = vec![
                FunctionDecl::print(),
                FunctionDecl::rand(),
                FunctionDecl::empty(),
            ];
            base.append(&mut FunctionDecl::num_funcs());
            base.append(&mut FunctionDecl::slice_funcs());
            base
        } //, time, rand],
        _ => vec![],
    }
}
pub fn get_native_types() -> Vec<ClassDecl> {
    return vec![ClassDecl::new(
        "slice".to_string(),
        vec![],
        vec![TypeParam::new("T".to_string(), "Any".to_string())],
    )];
}

pub fn get_any_trait() -> TraitDecl {
    TraitDecl::new("Any".to_string(), vec![], HashMap::new())
}
