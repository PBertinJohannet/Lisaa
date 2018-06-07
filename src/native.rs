use statement::{FunctionDecl, Statement};
use types::{LisaaType, TypedVar};
use vm::OP;

trait NativeFunc {
    fn print() -> FunctionDecl;
    fn rand() -> FunctionDecl;
    fn num_funcs() -> Vec<FunctionDecl>;
    fn newslice() -> FunctionDecl;
}

impl NativeFunc for FunctionDecl {
    fn print() -> Self {
        FunctionDecl {
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
            inline: true,
            name: "rand".to_owned(),
            type_args: vec![],
            args: vec![],
            scope: Statement::Native(vec![OP::RandNum]),
            ret_type: LisaaType::Num,
        }
    }
    fn newslice() -> Self {
        FunctionDecl {
            inline: true,
            name: "newslice".to_owned(),
            type_args: vec![],
            args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
            scope: Statement::Native(vec![OP::AllocObj]),
            ret_type: LisaaType::slice(LisaaType::Any),
        }
    }
    fn num_funcs() -> Vec<Self> {
        vec![
            FunctionDecl {
                inline: true,
                name: "num::add".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Add]),
                ret_type: LisaaType::Num,
            },
            FunctionDecl {
                inline: true,
                name: "num::times".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Mul]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::minus".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Neg, OP::Add]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::divide".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Inv, OP::Mul]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::not".to_owned(),
                type_args: vec![],
                args: vec![],
                scope: Statement::Native(vec![OP::Not]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::negate".to_owned(),
                type_args: vec![],
                args: vec![],
                scope: Statement::Native(vec![OP::Neg]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::ne".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Eq, OP::Not]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::equals".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Eq]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            /// we have a > b
            /// a, b
            /// push a, b
            /// a, b, a, b
            /// eq
            /// a, b, a==b,
            /// swap 3
            /// a==b, a, b
            /// gt
            /// a==b, a > b
            ///
            FunctionDecl {
                inline: true,
                name: "num::ge".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::GreaterEq]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::le".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::LowerEq]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::less".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::LowerThan]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::greater".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::GreaterThan]),
                ret_type: LisaaType::slice(LisaaType::Num),
            },
            FunctionDecl {
                inline: true,
                name: "num::index".to_owned(),
                type_args: vec![],
                args: vec![TypedVar::new(LisaaType::Num, "n".to_string())],
                scope: Statement::Native(vec![OP::Add]),
                ret_type: LisaaType::slice(LisaaType::Num),
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
                FunctionDecl::newslice(),
            ];
            base.append(&mut FunctionDecl::num_funcs());
            base
        } //, time, rand],
        _ => vec![],
    }
}
