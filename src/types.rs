
use std::fmt;
#[derive(Debug, Clone, PartialEq, Eq)]
/// This represents a class
/// Classes are not supported yet so it is useless.
pub struct Class {
    name: String,
}
#[derive(Debug, Clone, PartialEq, Eq)]
/// Represents the possible types of the language.
pub enum LisaaType {
    /// Represents a pointer to a heap allocated ressource.
    Pointer(Box<LisaaType>),
    /// Represents a Class
    Class(Class),
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
    /// An unresolved type found in the wild -> must be expanded to a class or type parameter.
    Unresolved(String),
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
    pub fn is_equivalent(&self, other : &Self) -> bool {
        if let (&LisaaType::Slice(ref lhs), &LisaaType::Slice(ref rhs)) = (self, other){
            return lhs.is_equivalent(rhs)
        } else if let (&LisaaType::Any, _) = (self, other){
            return true
        } else if let (_, &LisaaType::Any) = (self, other){
            return true
        } else {
            return self == other;
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
            &LisaaType::Unresolved(ref str) => write!(f, "{}", str),
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
