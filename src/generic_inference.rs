use expression::{BinaryExpr, Callee, Expr, ExprEnum, FunctionCall, Operator, UnaryExpr};
use native::get_native_types;
use statement::{
    Assignment, ClassDecl, Declaration, FunctionDecl, FunctionSig, IfStatement, Program, Statement,
    TraitDecl, TypeParam, WhileStatement,
};
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use types::{LisaaType, TypedVar};

///
/// Attempt at a module for generic type inference !
/// Given inforamtions on functions etc... find a function matching the given parameters.
pub struct Inferer<'a> {
    functions: &'a HashMap<FunctionSig, FunctionDecl>,
    given_argument_types: Vec<LisaaType>,
    given_generics: &'a Vec<LisaaType>,
    callee_type: Option<LisaaType>,
    func_name: String,
    traits: &'a HashMap<String, HashMap<String, FunctionSig>>,
}

impl<'a> Inferer<'a> {
    pub fn new(
        functions: &'a HashMap<FunctionSig, FunctionDecl>,
        local_functions: &'a HashSet<FunctionSig, FunctionDecl>,
        given_argument_types: Vec<LisaaType>,
        given_generics: &'a Vec<LisaaType>,
        callee_type: Option<LisaaType>,
        func_name: String,
        traits: &'a HashMap<String, HashMap<String, FunctionSig>>,
    ) -> Self {
        Inferer {
            functions: functions,
            given_argument_types: given_argument_types,
            given_generics: given_generics,
            callee_type: callee_type,
            func_name: func_name,
            traits: traits,
        }
    }
    /// How does it work...
    /// three possibilities :
    /// 1 The arguments are given : check number and trait bounds.
    /// 2 The arguments are not given : check all functions bound.
    pub fn infer(&self) -> Result<FunctionSig, String> {
        for (f, d) in self.functions {
            if f.name() == &self.func_name {
                match self.is_match(f) {
                    Some(f) => return Ok(f),
                    None => (),
                };
            }
        }
        Err(format!(
            "could not find a function satisfying type constraints for : {:?}",
            self.func_name
        ))
    }

    /// Try to match the two functions :
    ///
    /// Returns the morphised signature
    pub fn is_match(&self, sig: &FunctionSig) -> Option<FunctionSig> {
        if !self.given_generics.is_empty() {
            if self.check_type_constraints(&sig.type_args, &self.given_argument_types) {
                Some(self.get_sig_from_generics(sig, self.given_generics.clone()))
            } else {
                None
            }
        } else if sig.type_args.len() == self.given_generics.len() {
            self.check_signature(sig)
        } else {
            None
        }
    }

    /// Returns a signature from a signature and the actual types given to the generics.
    pub fn get_sig_from_generics(&self, orig: &FunctionSig, actual: Vec<LisaaType>) -> FunctionSig {
        let generics = &orig.type_args;
        // then use the generics to identify the return type.
        let ret_type = self.replace_gen(&orig.ret_type, generics, &actual);
        // then use the generics to identify the parameters.
        let actual_args = self
            .given_argument_types
            .iter()
            .map(|arg| self.replace_gen(arg, generics, &actual))
            .collect();
        FunctionSig::new_simple_args(vec![], actual_args, ret_type, orig.name().clone())
    }

    /// Checks the two signatures and then returns the actualised signature
    /// 1 => we find the generic in the function's signature (T : Add).
    /// 2 => we find where it should appear in the function's signature (T a, T b).
    /// 3 => we check that the arguments given are the same and respect the constraints.
    /// 4 => we rewrite the signature without the generic type.
    pub fn check_signature(&self, sig: &FunctionSig) -> Option<FunctionSig> {
        let generics = &sig.type_args;
        let mut actual_types = vec![];
        // first identify the types using the args.
        for g in generics {
            match self.check_generic_appearance(sig, g) {
                None => return None,
                Some(t) => actual_types.push(t),
            }
        }
        Some(self.get_sig_from_generics(sig, actual_types))
    }

    /// Given a type, returns an actual type if the it is a generic
    pub fn replace_gen(
        &self,
        orig: &LisaaType,
        gens: &Vec<TypeParam>,
        actual: &Vec<LisaaType>,
    ) -> LisaaType {
        match orig {
            LisaaType::Class(s, generics) => match actual
                .iter()
                .enumerate()
                .find(|&(id, _)| gens[id].name() == s)
            {
                Some((_, g)) => g.clone(),
                None => LisaaType::Class(
                    s.clone(),
                    generics
                        .iter()
                        .map(|g| self.replace_gen(g, gens, actual))
                        .collect(),
                ),
            },
            e => e.clone(),
        }
    }

    /// Given a generic parameter, checks that it appears correctly in the function :
    /// 1 It should appear with the same type everytime (eg (T a, T b) => (1, 2)).
    /// 2 It should appear at least once in the arguments.
    /// 3 It should respect the type constraints.
    pub fn check_generic_appearance(&self, sig: &FunctionSig, g: &TypeParam) -> Option<LisaaType> {
        let mut appearances = vec![];
        for (id, arg) in sig.args.iter().enumerate() {
            // find all occurences of that generic
            if let &LisaaType::Class(ref name, _) = arg {
                if name == g.name() {
                    appearances.push(self.given_argument_types[id].clone());
                }
            }
        }
        appearances.dedup_by(|a, b| a == b); // if all occurences are the same it should be length one
        if appearances.len() != 1 || !self.check_type_constraint(&g, &appearances[0]) {
            // checks the bounds
            None
        } else {
            Some(appearances[0].clone())
        }
    }

    /// Checks that a list type respects the bounds of a list of generics :
    pub fn check_type_constraints(
        &self,
        type_params: &Vec<TypeParam>,
        type_args: &Vec<LisaaType>,
    ) -> bool {
        type_params
            .iter()
            .zip(type_args.iter())
            .all(|(p, a)| self.check_type_constraint(p, a))
    }

    /// Checks that a type respects the bound on a generic :
    /// TODO: actually check that the bounds are respected.
    pub fn check_type_constraint(&self, type_param: &TypeParam, arg: &LisaaType) -> bool {
        for method in self.traits.get(type_param.trait_name()).iter() {
            println!("respect : {:?}", method);
        }
        true
    }
}
