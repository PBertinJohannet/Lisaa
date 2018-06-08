use statement::{ClassDecl, FunctionDecl};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
/// Represents the possible types of the language.
pub enum LisaaType {
    /// Represents a pointer to a heap allocated ressource.
    Pointer(Box<LisaaType>),
    /// Represents a Class
    Class(String),
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
    pub fn method(inner: LisaaType, name: String) -> Self {
        LisaaType::Function(format!("{}::{}", inner, name), vec![])
    }
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
        functions: &HashMap<String, FunctionDecl>,
    ) -> Result<LisaaType, String> {
        match self {
            &LisaaType::Pointer(ref inner) => inner.get_attr(name, classes, functions),
            &LisaaType::Num => Ok(LisaaType::Function(format!("num::{}", name), vec![])),
            &LisaaType::Char => Ok(LisaaType::Function(format!("num::{}", name), vec![])),
            &LisaaType::Slice(ref inner) => Ok(LisaaType::Function(
                format!("slice::{}", name),
                vec![*inner.clone()],
            )),
            &LisaaType::Class(ref s) => Ok(LisaaType::pointer(match classes.get(s) {
                Some(class) => match class.get_attr(name) {
                    Some(decl) => Ok(decl.val_type().clone()),
                    None => match functions.get(&format!("{}::{}", s, name)) {
                        Some(f) => Ok(LisaaType::Function(f.name().clone(), vec![])),
                        None => Err(format!("Can't take attribute {} of class {}", name, s)),
                    },
                },
                None => Err(format!("Unknown class : {}", s)),
            }?)),
            _ => Err(format!("can not get attr {} of {}", name, self)),
        }
    }
    /// Returns the attribute's type. Panics if it doesnt exist.
    pub fn get_attr_index(&self, name: &String, classes: &HashMap<String, ClassDecl>) -> usize {
        match self {
            &LisaaType::Class(ref s) => classes
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
            &LisaaType::Class(_) => vec![],
            &LisaaType::Pointer(ref p) => p.type_args(),
            _ => panic!("ok im out "),
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
            &LisaaType::Class(ref c) => write!(f, "class"),
            &LisaaType::TypeArg(ref str) => write!(f, "{}", str),
            &LisaaType::Function(ref str, _) => write!(f, "{}", str),
        }
    }
}

/// A variable associated with a type.
#[derive(Debug, Clone)]
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
    /// This is a num
    pub fn num(name: String) -> Self {
        TypedVar {
            type_var: Some(LisaaType::Num),
            name: name,
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
