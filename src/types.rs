use expression::{Expr, FunctionCall};
use statement::{ClassDecl, FunctionDecl, FunctionSig, TypeParam};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents the possible types of the language.
pub enum LisaaType {
    /// Represents a pointer to a heap allocated ressource.
    Pointer(Box<LisaaType>),
    /// Represents a Class with its type parameters
    Class(String, Vec<LisaaType>),
    /// A simple number (can be heap/stack)
    Num,
    /// A Char (heap/stack too)
    Char,
    /// An array of the given type allocated on the heap.
    Slice(Box<LisaaType>),
    /// Nothing.
    Void,
    /// Any type the legendary void*
    Any,
    /// THe type argument with number usize.
    TypeArg(usize),
    /// A method, represented by the type :: the name.
    Function(String, Vec<LisaaType>),
}

impl LisaaType {
    /// Creates a pointer pointing to the given type
    pub fn pointer(inner: LisaaType) -> Self {
        LisaaType::Pointer(Box::new(inner))
    }
    /// Creates a slice of the given type.
    pub fn slice(inner: LisaaType) -> Self {
        LisaaType::Slice(Box::new(inner))
    }

    pub fn function_name(&self) -> Result<String, ()> {
        match self {
            &LisaaType::Function(ref s, _) => Ok(s.to_owned()),
            &LisaaType::Pointer(ref inner) => Ok(inner.function_name()?),
            _ => Err(()),
        }
    }
    /// Dereferences if it is a pointer until it is not a pointer anymore
    /// Returns its type + the number of derefs
    pub fn max_deref(&self) -> (Self, usize) {
        let (mut a, mut i) = (self.clone(), 0);
        while let LisaaType::Pointer(box val) = a {
            a = val;
            i += 1;
        }
        (a, i)
    }
    /// Tells if a type is equivalent to another
    pub fn is_equivalent(&self, other: &Self) -> bool {
        if let (&LisaaType::Slice(ref lhs), &LisaaType::Slice(ref rhs)) = (self, other) {
            return lhs.is_equivalent(rhs);
        } else if let (&LisaaType::Any, _) = (self, other) {
            return true;
        } else if let (_, &LisaaType::Any) = (self, other) {
            return true;
        } else {
            return self == other;
        }
    }
    /// Returns the attribute's type if it exists
    pub fn get_attr(
        &self,
        name: &String,
        classes: &HashMap<String, ClassDecl>,
        functions: &HashMap<FunctionSig, FunctionDecl>,
    ) -> Result<LisaaType, String> {
        match self {
            &LisaaType::Pointer(ref inner) => inner.get_attr(name, classes, functions),
            &LisaaType::Num => Ok(LisaaType::Function(format!("num::{}", name), vec![])),
            &LisaaType::Char => Ok(LisaaType::Function(format!("num::{}", name), vec![])),
            &LisaaType::Slice(ref inner) => Ok(LisaaType::Function(
                format!("slice::{}", name),
                vec![*inner.clone()],
            )),
            &LisaaType::Class(ref s, ref t) => Ok(LisaaType::pointer(match classes.get(s) {
                Some(class) => match class.get_attr(name) {
                    Some(decl) => Ok(decl.val_type().clone()),
                    None => Self::get_class_attr(s, name, functions),
                },
                None => Err(format!("Unknown class : {}", s)),
            }?)),
            _ => Err(format!("can not get attr {} of {}", name, self)),
        }
    }
    /// If the type is a typevar checks that it is ok
    pub fn check_typevar(&self, type_params: &Vec<TypeParam>) -> Self {
        //if let LisaaType::Class(s)
        self.clone()
    }

    /// Returns the attr of the class given the class name
    pub fn get_class_attr(
        class: &String,
        attr: &String,
        functions: &HashMap<FunctionSig, FunctionDecl>,
    ) -> Result<LisaaType, String> {
        match functions
            .iter()
            .find(|(func, _)| func.name() == &format!("{}::{}", class, attr))
        {
            Some(f) => Ok(LisaaType::Function(f.0.name().clone(), vec![])),
            None => Err(format!("Can't take attribute {} of class {}", attr, class)),
        }
    }

    /// Returns the attribute's type. Panics if it doesnt exist.
    pub fn get_attr_index(&self, name: &String, classes: &HashMap<String, ClassDecl>) -> usize {
        match self {
            &LisaaType::Class(ref s, ref t) => classes
                .get(s)
                .expect("class not found")
                .get_attr_index(name),
            _ => panic!(format!("can not get attr {} of {}", name, self)),
        }
    }

    pub fn type_args(&self) -> Vec<LisaaType> {
        match self {
            &LisaaType::Slice(ref u) => vec![*u.clone()],
            &LisaaType::Num => vec![],
            &LisaaType::Char => vec![],
            &LisaaType::Class(_, ref t) => t.clone(),
            &LisaaType::Pointer(ref p) => p.type_args(),
            _ => panic!("ok im out "),
        }
    }

    pub fn get_constructor_call(&self, args: Vec<Expr>) -> FunctionCall {
        match self {
            &LisaaType::Slice(ref u) => {
                FunctionCall::constructor("slice".to_string(), args, vec![*u.clone()])
            }
            &LisaaType::Num => FunctionCall::constructor("num".to_string(), args, vec![]),
            &LisaaType::Char => FunctionCall::constructor("char".to_string(), args, vec![]),
            &LisaaType::Class(ref c, ref t) => {
                FunctionCall::constructor(c.clone(), args, t.clone())
            }
            &LisaaType::Pointer(ref p) => panic!("no constructor on pointers..."),
            _ => panic!("no constructor on other types."),
        }
    }
}

impl fmt::Display for LisaaType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LisaaType::Char => write!(f, "char"),
            &LisaaType::Num => write!(f, "num"),
            &LisaaType::Slice(ref u) => write!(f, "slice<{}>", u),
            &LisaaType::Void => write!(f, "void"),
            &LisaaType::Pointer(ref p) => write!(f, "&{}", p),
            &LisaaType::Any => write!(f, "any"),
            &LisaaType::Class(ref c, ref t) => write!(f, "class {}<{:?}>", c, t),
            &LisaaType::TypeArg(ref str) => write!(f, "{}", str),
            &LisaaType::Function(ref str, _) => write!(f, "{}", str),
        }
    }
}

/// A variable associated with a type.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypedVar {
    type_var: Option<LisaaType>,
    name: String,
}

impl TypedVar {
    /// Creates a new variable with the type.
    pub fn new(type_var: LisaaType, name: String) -> TypedVar {
        TypedVar {
            name: name,
            type_var: Some(type_var),
        }
    }
    /// Returns the name of the variable.
    pub fn name(&self) -> &str {
        &self.name
    }
    /// Returns the type of the variable.
    pub fn type_var(&self) -> &Option<LisaaType> {
        &self.type_var
    }
}
