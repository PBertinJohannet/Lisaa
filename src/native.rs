use statement::{FunctionDecl, Statement};
use types::{LisaaType, TypedVar};
use vm::OP;

/// A native function.
trait NativeFunc {
    fn print() -> FunctionDecl;
    fn rand() -> FunctionDecl;
    fn num_funcs() -> Vec<FunctionDecl>;
    fn slice_funcs() -> Vec<FunctionDecl>;
}

impl NativeFunc for FunctionDecl {
    fn print() -> Self {
        FunctionDecl {
            self_type: None,
            inline: true,
            name: "print".to_owned(),
            type_args: vec![],
            args: vec![TypedVar::new(LisaaType::Char, "a".to_owned())],
            scope: Statement::Native(vec![OP::PrintChar, OP::PushNum(0.0)]),
            ret_type: LisaaType::Num,
        }
    }
    fn rand() -> Self {
        FunctionDecl {
            self_type: None,
            inline: true,
            name: "rand".to_owned(),
            type_args: vec![],
            args: vec![],
            scope: Statement::Native(vec![OP::RandNum]),
            ret_type: LisaaType::Num,
        }
    }
    fn slice_funcs() -> Vec<Self> {
        vec![
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "newslice".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::AllocSlice]),
                ret_type: LisaaType::slice(LisaaType::Any),
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "slice::index".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Add]),
                ret_type: LisaaType::TypeArg(0),
            },
        ]
    }
    fn num_funcs() -> Vec<Self> {
        vec![
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::toString".to_owned(),
                type_args: vec![],
                args: vec![],
                scope: Statement::Native(vec![OP::ToStr]),
                ret_type: LisaaType::Class("String".to_owned()),
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::add".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Add]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::times".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Mul]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::modulo".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Swap2, OP::Mod]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::andand".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::AndAnd]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::oror".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::OrOr]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::or".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Or]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::minus".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Neg, OP::Add]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::divide".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Inv, OP::Mul]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::not".to_owned(),
                type_args: vec![],
                args: vec![],
                scope: Statement::Native(vec![OP::Not]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::negate".to_owned(),
                type_args: vec![],
                args: vec![],
                scope: Statement::Native(vec![OP::Neg]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::ne".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Eq, OP::Not]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::equals".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Eq]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::ge".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::LowerEq]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::le".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::GreaterEq]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::less".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::GreaterThan]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::greater".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::LowerThan]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                self_type: None,
                inline: true,
                name: "num::index".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Add]),
                ret_type: LisaaType::Num,
            },
        ]
    }
}

pub fn get_native_types(library: &str) -> Vec<FunctionDecl> {
    match library {
        "base" => {
            let mut base = vec![
                FunctionDecl::print(),
                FunctionDecl::rand(),
            ];
            base.append(&mut FunctionDecl::num_funcs());
            base.append(&mut FunctionDecl::slice_funcs());
            base
        } //, time, rand],
        _ => vec![],
    }
}
