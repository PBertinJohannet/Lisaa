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
    /// Nothing.
    Void,
    /// Any type
    Any,
    /// A method, represented by the caller :: the type :: the name.
    Function(Box<LisaaType>, String, Vec<LisaaType>),
}

impl LisaaType {
    /// Creates a pointer pointing to the given type
    pub fn pointer(inner: LisaaType) -> Self {
        LisaaType::Pointer(Box::new(inner))
    }
    /// Creates a slice of the given type.
    pub fn slice(inner: LisaaType) -> Self {
        LisaaType::Class("slice".to_string(), vec![inner])
    }

    pub fn function_name(&self) -> Result<String, ()> {
        match self {
            &LisaaType::Function(_, ref s, _) => Ok(s.to_owned()),
            &LisaaType::Pointer(ref inner) => Ok(inner.function_name()?),
            _ => Err(()),
        }
    }
    /// Returns the name used in method calls (eg num::add or Point::add).
    pub fn name(&self) -> String {
        match self {
            &LisaaType::Char => "char".to_string(),
            &LisaaType::Num => "num".to_string(),
            &LisaaType::Void => "void".to_string(),
            &LisaaType::Pointer(ref p) => format!("&{}", p),
            &LisaaType::Any => "any".to_string(),
            &LisaaType::Class(ref c, ref t) => c.to_string(),
            &LisaaType::Function(_, ref s, _) => s.to_string(),
        }
    }

    /// The function type
    pub fn function_type(&self) -> Result<LisaaType, ()> {
        match self {
            &LisaaType::Function(ref t, _, _) => Ok(*t.clone()),
            &LisaaType::Pointer(ref inner) => Ok(inner.function_type()?),
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
        if let (&LisaaType::Any, _) = (self, other) {
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
            &LisaaType::Num => Ok(LisaaType::Function(
                Box::new(self.clone()),
                format!("num::{}", name),
                vec![],
            )),
            &LisaaType::Char => Ok(LisaaType::Function(
                Box::new(self.clone()),
                format!("num::{}", name),
                vec![],
            )),
            &LisaaType::Class(ref s, ref t) => Ok(LisaaType::pointer(match classes.get(s) {
                Some(class) => match class.get_attr(name) {
                    Some(decl) => Ok(Self::morphise_attr(decl.val_type(), class, t)),
                    None => self.get_class_attr(s, name, functions),
                },
                None => Err(format!("Unknown class : {}", s)),
            }?)),
            _ => Err(format!("can not get attr {} of {}", name, self)),
        }
    }

    /// Morphise an attribute : given the return type the class decl and the actual type parameter
    /// eg : U, Point<T, U>, [num, str] -> str
    pub fn morphise_attr(
        type_found: &LisaaType,
        class_decl: &ClassDecl,
        actual_type_params: &Vec<LisaaType>,
    ) -> LisaaType {
        if let LisaaType::Class(name, _) = type_found {
            return match class_decl
                .type_params()
                .iter()
                .enumerate()
                .find(|(id, param)| param.name() == name)
            {
                Some((id, _)) => actual_type_params[id].clone(),
                None => type_found.clone(),
            };
        }
        type_found.clone()
    }

    /// If the type is a typevar checks that it is ok
    pub fn check_typevar(&self, type_params: &Vec<TypeParam>) -> Self {
        //if let LisaaType::Class(s)
        self.clone()
    }

    /// Returns the attr of the class given the class name
    pub fn get_class_attr(
        &self,
        class: &String,
        attr: &String,
        functions: &HashMap<FunctionSig, FunctionDecl>,
    ) -> Result<LisaaType, String> {
        println!("\n\n\n\nfunctions : {:?}", functions.iter().map(|(s, d)|s.name()).collect::<Vec<&String>>());
        match functions
            .iter()
            .find(|(func, _)| func.name() == &format!("{}::{}", class, attr))
        {
            Some(f) => Ok(LisaaType::Function(
                Box::new(self.clone()),
                f.0.name().clone(),
                vec![],
            )),
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
            &LisaaType::Num => vec![],
            &LisaaType::Char => vec![],
            &LisaaType::Class(_, ref t) => t.clone(),
            &LisaaType::Pointer(ref p) => p.type_args(),
            _ => panic!("ok im out "),
        }
    }

    pub fn get_constructor_call(&self, args: Vec<Expr>) -> FunctionCall {
        match self {
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
            &LisaaType::Void => write!(f, "void"),
            &LisaaType::Pointer(ref p) => write!(f, "&{}", p),
            &LisaaType::Any => write!(f, "any"),
            &LisaaType::Class(ref c, ref t) => write!(f, "class {}<{}>", c, t.iter().map(|i|format!("{}", i)).collect::<Vec<String>>().join(", ").replace("class ", "").replace("<>", "")),
            &LisaaType::Function(_, ref str, _) => write!(f, "{}", str),
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
