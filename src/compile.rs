//! Module for emiting bytecode readable by the vm.

use expression::{BinaryExpr, Expr, ExprEnum, FunctionCall, LiteralExpr, UnaryExpr};
use native::get_native_types;
use operations::{BinaryOperations, UnaryOperations};
use statement::{Assignment, Declaration, FunctionDecl, IfStatement, Statement, StatementResult,
                TypedVar, WhileStatement};
use std::collections::HashMap;
use token::TokenType;
use vm::OP;

/// These are unlinked instructions.
/// the goto (symbol) will be replaced by goto(usize) when the program is completed.
/// The Push(symbol) will be replaced by PushNum(float) when the program is completed.
#[derive(Clone, Debug)]
enum UnlinkedInstruction {
    Op(OP),
    Goto(String),
    Push(String),
}

/// Represents a scope with its variables.
/// Local variables are saved by their relative position on the stack.
///
#[derive(Debug)]
struct Scope {
    /// Associates the name of the variable with the position in the stack.
    variables: HashMap<String, usize>,
    /// This represents the number of scopes that shares variables with this one.
    depth: usize,
    /// This represents the current size occupied by this scope.
    current_size: usize,
    /// This represents the size of the stack when the scope started.
    starting_size: usize,
    /// End label, here only if the scope can be breaked (loops etc...).
    end_label: Option<String>,
}
impl Scope {
    /// Creates a new scope.
    pub fn new(depth: usize, starting_size: usize) -> Self {
        Scope {
            depth: depth,
            variables: HashMap::new(),
            starting_size: starting_size,
            current_size: starting_size,
            end_label: None,
        }
    }
    /// Creates a new scope.
    pub fn new_breakable(depth: usize, starting_size: usize, end_label: String) -> Self {
        Scope {
            depth: depth,
            variables: HashMap::new(),
            starting_size: starting_size,
            current_size: starting_size,
            end_label: Some(end_label),
        }
    }
    /// Returns true if the loop is breakable.
    pub fn is_breakable(&self) -> bool {
        self.end_label.is_some()
    }

    pub fn get_label(&self) -> String {
        self.end_label.clone().unwrap()
    }

    /// Returns the given variable.
    pub fn get_var(&self, var_name: &str) -> Option<usize> {
        self.variables.get(var_name).map(|u| u.clone())
    }
    /// Creates a new variable with the given name.
    pub fn create_var(&mut self, name: String) {
        self.current_size += 1;
        self.variables.insert(name, self.current_size - 1);
    }
}

/// The type checker
/// Contains a programm and functions to resolve types/verify consistency.
pub struct Compiler {
    code: Vec<UnlinkedInstruction>,
    functions: HashMap<String, FunctionDecl>,
    scopes: Vec<Scope>,
    /// associates the labels with the positions in the code.
    labels: HashMap<String, Option<usize>>,
}

impl Compiler {
    /// Creates a new typechekcer object.
    pub fn new() -> Self {
        Compiler {
            code: vec![],
            scopes: vec![],
            functions: HashMap::new(),
            labels: HashMap::new(),
        }
    }
    /// Returns the current stack's size.
    pub fn current_stack_size(&self) -> usize {
        self.scopes.last().unwrap().current_size
    }
    /// Add a lib to the program.
    pub fn add_lib(&mut self, lib: &str) {
        unimplemented!()
        /*for f in get_native_types(lib) {
            self.native_functions.insert(f.name(), f.args());
        }*/
    }
    /// Creates a variable in the current scope.
    pub fn create_var(&mut self, var: String) {
        self.scopes.last_mut().unwrap().create_var(var);
    }
    /// Finds the variable in the scope or the scope of its parent.
    pub fn get_var(&self, var_name: &str) -> Option<usize> {
        let current_scope = self.scopes.last().unwrap().depth;
        let len = self.scopes.len();
        for sc in 0..current_scope + 1 {
            if self.scopes[len - sc - 1].get_var(var_name).is_some() {
                return self.scopes[len - sc - 1].get_var(var_name);
            }
        }
        None
    }

    /// Creates a new label with the given name at the given position in the code.
    pub fn new_label_here(&mut self, s: String) {
        self.labels.insert(s, Some(self.code.len()));
    }

    /// Creates a new label wich position is not known.
    pub fn new_empty_label(&mut self) -> String {
        let lab_name = self.labels.len().to_string();
        self.labels.insert(lab_name.clone(), None);
        lab_name
    }

    /// Sets the name of the label.
    pub fn label_here(&mut self, label: String) {
        *self.labels.get_mut(&label).unwrap() = Some(self.code.len());
    }

    pub fn emit(&mut self, op: OP) {
        self.code.push(UnlinkedInstruction::Op(op));
    }

    pub fn emit_chunks(&mut self, mut ops: Vec<OP>) {
        for o in ops {
            self.emit(o);
        }
    }

    pub fn emit_goto(&mut self, s: String) {
        self.code.push(UnlinkedInstruction::Goto(s));
    }

    pub fn emit_push(&mut self, s: String) {
        self.code.push(UnlinkedInstruction::Push(s));
    }

    /// Resolve types if possible
    /// The aim is to traverse the tree and resolve the return type of all expressions.
    pub fn compile(&mut self, program: &HashMap<String, FunctionDecl>) -> Result<Vec<OP>, String> {
        //self.add_lib("base");
        self.functions = program.clone();

        self.function_call(&FunctionCall::new("main".to_string(), vec!()));
        self.emit(OP::End);
        for f in program.iter(){
            self.function(f.1);
        }
        self.compile_std();
        println!("code : {:?}", self.code);
        Ok(self.code
            .iter()
            .map(|e| match e {
                &UnlinkedInstruction::Op(ref o) => o.clone(),
                &UnlinkedInstruction::Goto(ref label) => {
                    OP::Goto(self.labels.get(label).unwrap().unwrap())
                },
                &UnlinkedInstruction::Push(ref label) => {
                    OP::PushNum(self.labels.get(label).unwrap().unwrap() as f64)
                }
            })
            .collect())
    }

    /// Compiles a function.
    /// The function assumes the following stack configurations :
    /// Ret | Ins | Off | Args ...
    /// Where Ret is the return value, Off is the stack offset of the preceding function,
    /// Ins is the index of the instruction to execute next and Args the arguments in order.
    /// The calling convention is emited in the function_calls.
    /// Puts a label for the begining of the function with the function name.
    pub fn function(&mut self, func: &FunctionDecl) {
        self.new_label_here(func.name().to_string());
        self.scopes.push(Scope::new(1, 0));
        self.create_var("0".to_string()); // return value.
        self.create_var("1".to_string()); // next instruction.
        self.create_var("2".to_string()); // offset.
        for var in func.args(){
            self.create_var(var.name().to_string());
        }
        for st in func.scope() {
            self.statement(st);
        }
        self.return_statement(&Expr::number(0.0, 1));
    }

    /// Compiles a statement. depends on the statement.
    pub fn statement(&mut self, statement: &Statement) {
        match statement {
            &Statement::Assignment(ref a) => self.assignment(a),
            &Statement::Declaration(ref d) => self.declaration(d),
            &Statement::ExprStatement(ref e) => {
                self.expression(e);
                self.emit(OP::Pop);
            },
            &Statement::Scope(ref s) => self.scope(s),
            &Statement::IfStatement(ref i) => self.if_statement(i),
            &Statement::WhileStatement(ref i) => self.while_statement(i),
            &Statement::BreakStatement => self.break_scope(),
            &Statement::ReturnStatement(ref e) => self.return_statement(e),
            ref a => panic!("other statements are not supported for now"),
        }
    }

    /// Compiles a return statement
    /// since the stack has the following configuration
    /// Ret | Ins | Off | Args ...
    /// We need to free everything except the Ret | Off then Sets the offset to the position of Off
    /// while consuming it.
    pub fn return_statement(&mut self, expression : &Expr){
        self.expression(expression);
        self.emit(OP::Set(0)); // sets return value.
        let to_pop =  self.scopes.last().unwrap().current_size-3;
        self.emit(OP::PopN(to_pop)); // pop the allocated variables.
        self.emit(OP::SetOffset); // Reset the offset to the last function.
        self.emit(OP::GotoTop); // goto the next instruction after the function call.
    }

    pub fn break_scope(&mut self){
        let id_last = self.get_last_breakable_scope_id();
        let to_pop =  self.scopes.last().unwrap().current_size - self.scopes[id_last].starting_size;
        let label_end = self.scopes[id_last].get_label();
        self.emit(OP::PopN(to_pop));
        self.emit_goto(label_end);
    }

    pub fn get_last_breakable_scope_id(&self) -> usize {
        self.scopes
            .iter()
            .enumerate()
            .rev()
            .filter(|&(i, a)| a.is_breakable())
            .next()
            .unwrap()
            .0
    }

    pub fn get_next_loop_start_and_depth(&self) -> (usize, usize) {
        (self.scopes.len(), self.scopes.last().unwrap().current_size)
    }

    /// Compiles a while statement by doing so :
    /// There is a label at the start and a label at the end.
    /// At the end of a statement a goto -> start.
    /// at the condition a goto -> end.
    ///
    /// The loop creates its own breakable scope with an end.
    ///
    pub fn while_statement(&mut self, while_statement: &WhileStatement) {
        let while_start = self.new_empty_label();
        let while_end = self.new_empty_label();
        // start of loop.
        self.label_here(while_start.clone());
        // condition
        self.expression(while_statement.condition());
        self.emit(OP::JMPIf);
        self.emit_goto(while_end.clone());
        // inner breakable scope
        let inner_loop_end = self.new_empty_label();
        let (depth, starting_size) = self.get_next_loop_start_and_depth();
        self.scopes.push(Scope::new_breakable(
            depth,
            starting_size,
            while_end.to_string(),
        ));
        // statement
        self.statement(while_statement.statement());
        self.emit_goto(while_start.clone());
        // end
        self.label_here(while_end);
    }

    pub fn if_statement(&mut self, if_statement: &IfStatement) {
        self.expression(if_statement.condition());
        let end_label = self.new_empty_label();
        self.emit(OP::JMPIf);
        self.emit_goto(end_label.clone());
        self.statement(if_statement.statement());
        self.label_here(end_label);
    }

    /// compiles a scope.
    pub fn scope(&mut self, scope: &Vec<Statement>) {
        let depth = self.scopes.len();
        let starting_size = self.scopes.last().unwrap().current_size;
        self.scopes.push(Scope::new(depth, starting_size));
        for st in scope {
            self.statement(st);
        }
        self.exit_scope();
    }

    /// Exits the scope and destroys all allocated variables in the stack.
    pub fn exit_scope(&mut self) {
        let last_scope = self.scopes.pop().unwrap();
        for i in 0..(last_scope.current_size - last_scope.starting_size) {
            self.emit(OP::Pop)
        }
    }

    /// Compiles a declaration.
    /// Declares the variable and run the expression.
    pub fn declaration(&mut self, decl: &Declaration) {
        self.create_var(decl.name().to_string());
        self.emit(OP::PushNum(0.0));
        self.assignment(decl.assignment());
    }

    /// Compiles assignment.
    ///
    pub fn assignment(&mut self, assignement: &Assignment) {
        self.expression(assignement.expr());
        let var = self.get_var(assignement.identifier()).unwrap();
        self.emit(OP::Set(var));
    }

    /// Compiles an expression.
    /// The result of the expression will be at the top of the stack at the end.
    pub fn expression(&mut self, expr: &Expr) {
        match expr.expr() {
            &ExprEnum::Literal(ref l) => self.literal(l),
            &ExprEnum::Unary(ref u) => self.unary(u),
            &ExprEnum::Binary(ref b) => self.binary(b),
            &ExprEnum::Identifier(ref i) => self.identifier(i),
            &ExprEnum::FunctionCall(ref f) => self.function_call(f),
            _ => panic!(format!("expression not supported {:?}", expr).to_string()),
        }
    }

    /// As specified, when a function is called we the stack must be in the following format :
    /// Ret | Off | Args ...
    /// So we push the ret, push the offset and go to the function.
    pub fn function_call(&mut self, call : &FunctionCall){
        let after_call = self.new_empty_label();
        self.emit(OP::PushOffset);// stack :  | Offset |
        self.emit(OP::PushNum(0.0)); // pushes the return value.
        self.emit(OP::Swap2); // swaps to get the return value under the offset.
        self.emit_push(after_call.to_string()); // push the value of the instructions after the call.
        self.emit(OP::Swap2); // Swaps the offset with the instruction pointer.
        for var in call.args(){
            self.expression(var);
        }
        self.emit(OP::OffsetToTop(call.args().len()+3)); // down the current offset to (num args + 3)
        self.emit_goto(call.name().to_string());
        self.label_here(after_call);
    }

    pub fn literal(&mut self, literal: &LiteralExpr) {
        match literal {
            &LiteralExpr::NUMBER(n) => self.emit(OP::PushNum(n)),
            _ => panic!("strings not supported yet"),
        }
    }

    pub fn identifier(&mut self, ident: &String) {
        let val = self.get_var(ident).unwrap();
        self.emit(OP::Bring(val));
    }

    pub fn unary(&mut self, unary: &UnaryExpr) {
        self.expression(unary.expression());
        match unary.operator().get_type() {
            &TokenType::MINUS => self.emit(OP::Neg),
            &TokenType::BANG => self.emit_chunks(vec![OP::Not]),
            _ => panic!("unexpected this"),
        }
    }

    /// We exchange lower than and greater than because the rhs is at the top of the stack.
    pub fn binary(&mut self, exp: &BinaryExpr) {
        // puts left hand side at the top
        self.expression(exp.lhs());
        // puts rhs at the top.
        self.expression(exp.rhs());
        match exp.operator().get_type() {
            &TokenType::MINUS => self.emit_chunks(vec![OP::Neg, OP::Add]),
            &TokenType::PLUS => self.emit(OP::Add),
            &TokenType::STAR => self.emit(OP::Mul),
            &TokenType::SLASH => self.emit_chunks(vec![OP::Inv, OP::Mul]),
            &TokenType::GreaterEqual => self.emit_chunks(vec![OP::LowerThan, OP::Eq, OP::Add]),
            &TokenType::GREATER => self.emit(OP::LowerThan),
            &TokenType::LessEqual => self.emit_chunks(vec![OP::GreaterThan, OP::Eq, OP::Add]),
            &TokenType::LESS => self.emit(OP::GreaterThan),
            &TokenType::EqualEqual => self.emit(OP::Eq),
            &TokenType::BangEqual => self.emit_chunks(vec![OP::Eq, OP::Not]),
            &TokenType::ANDAND => self.emit_chunks(vec![OP::And]),
            e => panic!(format!("operator {:?} can not be aplied to two value", e)),
        }
    }

    pub fn compile_std(&mut self){
        self.compiled_print();
        self.compiled_rand();
    }

    pub fn compiled_print(&mut self){
        self.new_label_here("print".to_string());
        self.emit_chunks(vec![OP::PrintNum, OP::SetOffset, OP::GotoTop]);
    }

    pub fn compiled_rand(&mut self){
        self.new_label_here("rand".to_string());
        self.emit_chunks(vec![OP::RandNum, OP::Set(0), OP::SetOffset, OP::GotoTop]);
    }
}

