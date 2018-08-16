use expression::{BinaryExpr, Callee, Expr, ExprEnum, FunctionCall, Operator, UnaryExpr};
use native::get_native_funcs;
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
#[derive(Debug)]
pub struct Inferer<'a> {
    functions: &'a HashMap<FunctionSig, FunctionDecl>,
    local_functions: &'a HashSet<FunctionSig>,
    given_argument_types: Vec<LisaaType>,
    given_generics: Vec<LisaaType>,
    callee_type: Option<LisaaType>,
    func_name: String,
    traits: &'a HashMap<String, HashMap<String, FunctionSig>>,
}

impl<'a> Inferer<'a> {
    pub fn new(
        functions: &'a HashMap<FunctionSig, FunctionDecl>,
        local_functions: &'a HashSet<FunctionSig>,
        given_argument_types: Vec<LisaaType>,
        given_generics: Vec<LisaaType>,
        callee_type: Option<LisaaType>,
        func_name: String,
        traits: &'a HashMap<String, HashMap<String, FunctionSig>>,
    ) -> Self {
        Inferer {
            functions: functions,
            local_functions: local_functions,
            given_argument_types: given_argument_types,
            given_generics: given_generics,
            callee_type: callee_type,
            func_name: func_name,
            traits: traits,
        }
    }

    /// Given enough information in the inferer find the correct function/mehtod to call.
    /// Returns the function alongside with the declaration, the original type parameters and the actual types given.
    pub fn infer(&self, line : usize) -> Result<(FunctionSig, Option<(&'a FunctionDecl, Vec<TypeParam>, Vec<LisaaType>)>), String> {
        for (f, d) in self.functions {
            if f.name() == &self.func_name && f.args.len() == self.given_argument_types.len() {
                match self.is_match(f) {
                    Some((f, gen, act)) => return Ok((f, Some((d, gen, act)))),
                    None => (),
                };
            }
        }
        for f in self.local_functions {
            if f.name() == &self.func_name && f.args.len() == self.given_argument_types.len(){
                match self.is_match(f) {
                    Some(f) => return Ok((f.0, None)),
                    None => (),
                };
            }
        }
        Err(format!(
            "could not find a function satisfying type constraints for : {:?} line : {}",
            self.func_name,
            line,
        ))
    }

    /// Try to match the two functions :
    ///
    /// Returns the morphised signature
    pub fn is_match(&self, sig: &FunctionSig) -> Option<(FunctionSig, Vec<TypeParam>, Vec<LisaaType>)> {
        if !self.given_generics.is_empty() && sig.type_args.len() == self.given_generics.len() {
            if self.check_type_constraints(&sig.type_args, &self.given_argument_types) {
                Some(self.get_sig_from_generics(sig, self.given_generics.clone()))
            } else {
                None
            }
        } else {
            self.check_signature(sig)
        }
    }

    /// Returns a signature from a signature and the actual types given to the generics.
    pub fn get_sig_from_generics(&self, orig: &FunctionSig, actual: Vec<LisaaType>) -> (FunctionSig, Vec<TypeParam>, Vec<LisaaType>){
        let generics = &orig.type_args;
        // then use the generics to identify the return type.
        let ret_type = Self::replace_gen(&orig.ret_type, generics, &actual);
        let self_type = orig
            .self_type
            .clone()
            .map(|t| Self::replace_gen(&t, generics, &actual));
        // then use the generics to identify the parameters.
        let actual_args = self
            .given_argument_types
            .iter()
            .map(|arg| Self::replace_gen(arg, generics, &actual))
            .collect();
        (FunctionSig::new_simple_args(
            vec![],
            actual_args,
            ret_type,
            orig.name().clone(),
            self_type,
        ), generics.clone(), actual)
    }

    /// Checks the two signatures and then returns the actualised signature
    /// 1 => we find the generic in the function's signature (T : Add).
    /// 2 => we find where it should appear in the function's signature (T a, T b).
    /// 3 => we check that the arguments given are the same and respect the constraints.
    /// 4 => we rewrite the signature without the generic type.
    pub fn check_signature(&self, sig: &FunctionSig) -> Option<(FunctionSig, Vec<TypeParam>, Vec<LisaaType>)> {
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
                        .map(|g| Self::replace_gen(g, gens, actual))
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
        if sig.self_type.is_some() {
            appearances.append(&mut self.check_generic_in_class(
                g.name(),
                &sig.self_type.clone().unwrap(),
                &self.callee_type.clone().unwrap(),
            ))
        }
        for (id, arg) in sig.args.iter().enumerate() {
            // find all occurences of that generic
            appearances.append(&mut self.check_generic_in_class(
                g.name(),
                arg,
                &self.given_argument_types[id],
            ))
        }
        appearances.dedup_by(|a, b| a == b); // if all occurences are the same it should be length one
        if appearances.len() != 1 || !self.check_type_constraint(&g, &appearances[0]) {
            // checks the bounds
            None
        } else {
            Some(appearances[0].clone())
        }
    }

    /// Checks that a generic appear in a type (eg T appear in Point<T> and T but not num).
    /// gen name is the name of the generic.
    /// arg is the argument required by the function
    /// given is the argument given to the function.
    /// given T, Point<T>, Point<Num> it should return [Num]
    pub fn check_generic_in_class(
        &self,
        gen_name: &String,
        arg: &LisaaType,
        given: &LisaaType,
    ) -> Vec<LisaaType> {
        if let &LisaaType::Pointer(ref i) = given {
            return self.check_generic_in_class(gen_name, arg, i);
        }
        if let &LisaaType::Class(ref name, ref generics) = arg {
            if gen_name == name {
                return vec![given.clone()];
            }
            let mut occurences = vec![];
            for (id, sub_arg) in generics.iter().enumerate() {
                if let LisaaType::Class(_, ref args) = given {
                    occurences
                        .append(&mut self.check_generic_in_class(gen_name, sub_arg, &args[id]));
                }
            }
            return occurences;
        }
        vec![]
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
    /// We need to find a method on the type that is ok
    /// eg : method add(Self) -> Self
    /// for each candidate : Point::add
    /// check that the function has first generic Self and return type self
    pub fn check_type_constraint(&self, type_param: &TypeParam, arg: &LisaaType) -> bool {
        self.traits
            .get(type_param.trait_name())
            .iter()
            .flat_map(|a| a.iter())
            .map(|(name, sig)| (format!("{}::{}", arg.name(), name), sig))
            .all(|(method_name, method_sig)| {
                //"check for : {:?}", method_name);
                for f in self.functions.iter().filter(|f| f.0.name() == &method_name) {

                    //println!("check : {:?}{:?}", f.0, method_sig);
                }
                true
            })
    }
}
