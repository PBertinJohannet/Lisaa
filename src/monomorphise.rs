use expression::{BinaryExpr, Callee, Expr, ExprEnum, FunctionCall, Operator, UnaryExpr};
use native::get_native_types;
use statement::{
    Assignment, ClassDecl, Declaration, FunctionDecl, FunctionSig, IfStatement, Program, Statement,
    TraitDecl, TypeParam, WhileStatement,
};
use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;
use types::{LisaaType, TypedVar};
/*
///Once we made sure that the program typechecks we can create all the functions that will need to
/// be called.
struct Monomorphiser {
    morphised_functions: HashMap<FunctionSig, FunctionDecl>,
    functions: HashMap<FunctionSig, FunctionDecl>,
    morphised_classes: HashMap<(String, Vec<LisaaType>), FunctionDecl>,
    classes: HashMap<String, ClassDecl>,
    current_replacements: HashMap<String, LisaaType>,
}

impl Monomorphiser {
    pub fn new(program: Program) -> Self {
        Monomorphiser {
            morphised_classes: HashMap::new(),
            morphised_functions: HashMap::new(),
            functions: program.functions().clone(),
            classes: program.classes().clone(),
            current_replacements: HashMap::new(),
        }
    }
    /// Dispatch the functions and return them
    pub fn dispatch(&mut self) -> Result<Program, String> {
        let main = self.functions.get(&FunctionSig::new(
            vec![],
            vec![],
            LisaaType::Void,
            "main".to_string(),
        ));
        if main.is_none() {
            return Err(format!("No main function found... wtf ?"));
        }
        self.dispatch_func(main.signature())?;
        Ok(Program::new(self.functions, self.classes, HashMap::new()))
    }
    /// Dispatch the function with the given signature :
    /// The function should never be generic in any ways.
    /// Clone the function decl and run through it with the given
    ///
    /// In order :
    /// When we encounter a function call
    /// take the functiondecl from the signature (howto?)
    ///
    pub fn dispatch_func(&mut self, func: FunctionDecl) -> Result<(), String> {
        if self.morphised_functions.contains_key(&func) {
            return Ok(());
        }
        let actual_func = self.functions.get(&func).unwrap();
        /*for s in actual_func.(){

        }*/
        Ok(())
    }
}
*/
