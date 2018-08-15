use rand::random;
use std::char;
#[allow(unused_imports)]
use std::io::{self, Write};
//mod gc;
mod allocator;
use self::allocator::Allocator;
pub use self::allocator::{IS_PTR_SLICE_BIT, IS_SLICE_BIT};
use std::collections::BTreeSet;
pub const STRING_TYPE: u64 = 6; // size 2 -> 4 + pointer in position 1 -> 2 = 6

#[derive(Debug, Clone)]
pub enum OP {
    /// This is the end of the program.
    End,
    Goto(usize),
    Pop,
    PopN(usize),
    /// Sets the offset of the stack to the value at the top (and consume the value)
    SetOffset,
    /// Sets the offset of the stack down.
    OffsetToTop(usize),
    /// Pushes the offset of the stack to the stack.
    PushOffset,
    /// Jumps to the instruction with the same value as the top of the stack
    GotoTop,
    Inv,
    Mul,
    Mod,
    GreaterThan,
    GreaterEq,
    Eq,
    Not,
    Or,
    OrOr,
    AndAnd,
    #[allow(dead_code)]
    And,
    LowerThan,
    LowerEq,
    Swap2,
    Bring(usize),
    Set(usize),
    /// Negates the current top of the stack.
    Neg,
    /// Add the top and second value of the stack.
    Add,
    /// allocate an object of known size to the heap and put its adress at the top of the stack
    /// after using it, to avoid leaking memory, always add the pointers offsets from the top :
    /// correct : Push(1.0) AllocObj Push(1.0) Add
    /// incorrect : Push(1.0), Push(1.0) AllocObj Add.
    /// In the incorrect case the reference is droped and will be freed on the next gc run...
    /// Takes the memory descriptor of the object to allocate it.
    AllocObj(u64),
    /// Allocates a slice, temporary...
    AllocSlice,
    /// Access the value in the heap and push it to the stack.
    GetHeap,
    /// Take the top of the stack as an adress to the heap and set its value to the second value in
    /// the  stack. eg -> if the stack is 3, 2 the object at adress 2 will take value 3.
    SetHeap,
    /// skip the next instruction if the top of the stack is 0.
    JMPIf,
    PushNum(f64),
    PushCopy,
    PrintChar,
    ToStr,
    RandNum,
}

/// no memory safety, everything leaks.
/// Stack contains only f64. everything else in the heap.
pub struct Vm<'a> {
    /// The root references into the heap. from the stack/global
    /// Contains a list of bools, the size of the stack.
    root_references: BTreeSet<usize>,
    //program : Vec<Vec<OP>>, matches functions to OPs.
    /// The stack contains ints/references in 64bit format.
    stack: Vec<f64>,
    /// The current offset of the stack
    stack_offset: usize,
    allocator: Allocator,
    output_stream: &'a mut Write,
}

impl<'a> Vm<'a> {
    pub fn new(output_stream: &'a mut Write) -> Vm {
        Vm {
            root_references: BTreeSet::new(),
            stack: vec![],
            stack_offset: 0,
            allocator: Allocator::new(),
            output_stream: output_stream,
        }
    }

    pub fn heap(&self) -> Vec<u64> {
        self.allocator.heap()
    }

    pub fn run(&mut self, program: Vec<OP>) {
        let mut instruction_pointer = 0;
        while instruction_pointer < program.len() {
            let op = &program[instruction_pointer];
            instruction_pointer += 1;
            //println!("executing {:?}", op);
            match op {
                &OP::End => {
                    //println!("program execution terminated");
                    return;
                },
                &OP::Goto(u) => instruction_pointer = u,
                &OP::GotoTop => instruction_pointer = self.stack.pop().unwrap() as usize,
                &OP::OffsetToTop(u) => {
                    self.stack_offset = self.stack.len()-u;
                }
                &OP::PushOffset => {
                    let val = self.stack_offset as f64;
                    self.stack.push(val);
                }
                &OP::SetOffset => {
                    let val = self.stack.pop().unwrap();
                    self.stack_offset = val as usize;
                }
                &OP::PopN(u) => {
                    let next_size = self.stack.len()-u;
                    for i in 0..u{
                        self.root_references.remove(&(self.stack.len()-1-i));
                    }
                    self.stack.truncate(next_size);
                }
                &OP::Pop => {
                    self.stack.pop();
                    self.root_references.remove(&self.stack.len());
                }
                &OP::Not => {
                    let val = match self.stack.pop().unwrap() == 0.0 {
                        false => 0.0,
                        _ => 1.0,
                    };
                    self.stack.push(val)
                }
                &OP::GreaterThan => {
                    let val = match self.stack.pop().unwrap() > self.stack.pop().unwrap() {
                        false => 0.0,
                        _ => 1.0,
                    };
                    self.stack.push(val)
                }
                &OP::GreaterEq => {
                    let val = (self.stack.pop().unwrap() >= self.stack.pop().unwrap()) as i32 as f64;
                    self.stack.push(val)
                }
                &OP::LowerEq => {
                    let val = (self.stack.pop().unwrap() <= self.stack.pop().unwrap()) as i32 as f64;
                    self.stack.push(val)
                }
                &OP::LowerThan => {
                    let val = (self.stack.pop().unwrap() < self.stack.pop().unwrap()) as i32 as f64;
                    self.stack.push(val)
                }
                &OP::And => {
                    let val1 = self.stack.pop().unwrap() as i64;
                    let val2 = self.stack.pop().unwrap() as i64;
                    let res = val1 & val2;
                    self.stack.push(res as f64)
                }
                &OP::AndAnd => {
                    let val1 = self.stack.pop().unwrap() != 0.0;
                    let val2 = self.stack.pop().unwrap() != 0.0;
                    let res = (val1 && val2) as i32 as f64;
                    self.stack.push(res)
                }
                &OP::Or => {
                    let val1 = self.stack.pop().unwrap() as i64;
                    let val2 = self.stack.pop().unwrap() as i64;
                    let res = val1 | val2;
                    self.stack.push(res as f64)
                }
                &OP::OrOr => {
                    let val1 = self.stack.pop().unwrap() != 0.0;
                    let val2 = self.stack.pop().unwrap() != 0.0;
                    let res = (val1 || val2) as i32 as f64;
                    self.stack.push(res)
                }
                &OP::Eq => {
                    let val = match self.stack.pop().unwrap() == self.stack.pop().unwrap() {
                        false => 0.0,
                        _ => 1.0,
                    };
                    self.stack.push(val)
                }
                &OP::PushNum(n) => self.stack.push(n),
                &OP::PushCopy => {
                    let top = self.stack.last().unwrap().clone();
                    self.stack.push(top);
                },

                &OP::PrintChar => {
                    write!(self.output_stream, "{}", char::from_u32(self.stack.pop().unwrap() as u32).unwrap()).unwrap();
                }
                &OP::ToStr => {
                    let top = self.stack.pop().unwrap().to_string();
                    let len = top.len();
                    let str_index = self.allocator.alloc(2, STRING_TYPE);
                    self.allocator.set_ptr(str_index, len as f64);
                    let slice_index = self.allocator.alloc(len, IS_SLICE_BIT+len as u64);
                    self.allocator.set_ptr(str_index+1, slice_index as f64);
                    for (i, ch )in top.chars().enumerate(){
                        self.allocator.set_ptr(slice_index+i, ch as u32 as f64);
                    }
                    self.stack.push(str_index as f64);
                    //println!("stack {:?}", self.stack);
                    //println!("heap {:?}", self.allocator.heap);
                }
                &OP::RandNum => {
                    self.stack.push(random::<f64>());
                }
                &OP::JMPIf => {
                    if self.stack.pop().unwrap() != 0.0 {
                        instruction_pointer += 1;
                    }
                }
                &OP::Inv => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(1.0 / val)
                }
                &OP::Mul => {
                    let val = self.stack.pop().unwrap() * self.stack.pop().unwrap();
                    self.stack.push(val)
                }
                &OP::Neg => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(-val)
                }
                &OP::Add => {
                    let val = self.stack.pop().unwrap() + self.stack.pop().unwrap();
                    self.stack.push(val)
                }
                &OP::Mod => {
                    let val = (self.stack.pop().unwrap() as i32) % (self.stack.pop().unwrap() as i32);
                    self.stack.push(val as f64)
                }
                &OP::Swap2 => {
                    let (high, low) = (self.stack.pop().unwrap(), self.stack.pop().unwrap());
                    self.stack.push(high);
                    self.stack.push(low);
                }
                &OP::Bring(id) => {
                    if self.root_references.contains(&(self.stack_offset+id)){
                        self.root_references.insert(self.stack.len());
                    }
                    let val = self.stack[self.stack_offset + id];
                    self.stack.push(val);
                }
                &OP::Set(id) => {
                    let to_set = self.stack.pop().unwrap();
                    if self.root_references.contains(&(self.stack.len())){
                        self.root_references.remove(&self.stack.len());
                        self.root_references.insert(self.stack_offset+id);
                    }
                    self.stack[self.stack_offset + id] = to_set;
                }
                &OP::AllocObj(descr) => {
                    self.allocator.run_gc();
                    let size = self.stack.pop().unwrap() as i32 as usize;
                    let val = self.allocator.alloc(size, descr);
                    self.root_references.insert(self.stack.len());
                    self.stack.push(val as f64);
                }
                &OP::AllocSlice => {
                    self.allocator.run_gc();
                    let size = self.stack.pop().unwrap() as i32 as usize;
                    let val = self.allocator.alloc(size, size as u64+IS_SLICE_BIT);
                    self.root_references.insert(self.stack.len());
                    self.stack.push(val as f64);
                }
                &OP::GetHeap => {
                    let adress: usize = self.stack.pop().unwrap() as usize;
                    let val: f64 = self.allocator.get_heap(adress) as f64;
                    self.root_references.insert(self.stack.len());
                    self.stack.push(val);
                }
                &OP::SetHeap => {
                    let (adress, value) = (
                        self.stack.pop().unwrap() as usize,
                        self.stack.pop().unwrap(),
                    );
                    self.root_references.remove(&(self.stack.len()+1));
                    //println!("val at : {} to {}", adress, value);
                    self.allocator.set_ptr(adress, value);
                }
                //_ => panic!("unsupported operand"),
            }
            /*println!("stack : {:?}", self.stack);
            println!(
                "root refs : {:?}",
                self.root_references.iter().collect::<Vec<&usize>>()
            );
            println!("heap : {:?}", self.allocator.heap());*/
        }
    }
}

#[cfg(test)]
mod tests_vm {
    use super::*;
    #[test]
    fn test_load() {
        let source = vec![OP::PushNum(1.0), OP::PrintChar];
        let mut stdout = io::stdout();
        let mut vm = Vm::new(&mut stdout);
        vm.run(source);
        assert_eq!(0, vm.stack.len());
        let source = vec![OP::PushNum(1.0), OP::PushNum(0.1)];
        vm.run(source);
        assert_eq!(2, vm.stack.len());
    }
    #[test]
    fn test_operations() {
        let source = vec![OP::PushNum(1.0), OP::PushNum(2.0), OP::Add, OP::Neg];
        let mut stdout = io::stdout();
        let mut vm = Vm::new(&mut stdout);
        vm.run(source);
        assert_eq!(1, vm.stack.len());
        assert_eq!(-3.0, vm.stack[0]);
        let source = vec![OP::PushNum(1.0), OP::Mul];
        vm.run(source);
        assert_eq!(-3.0, vm.stack[0]);
        let source = vec![OP::PushNum(1.0)];
        vm.run(source);
        assert_eq!(-3.0, vm.stack[0]);
    }
    #[test]
    fn test_swap() {
        let source = vec![
            OP::PushNum(1.0),
            OP::PushNum(2.0),
            OP::PushNum(0.5),
            OP::Swap2,
        ];
        let mut stdout = io::stdout();
        let mut vm = Vm::new(&mut stdout);
        vm.run(source);
        assert_eq!(3, vm.stack.len());
        assert_eq!(1.0, vm.stack[0]);
        assert_eq!(0.5, vm.stack[1]);
        let source = vec![OP::Swap2];
        vm.run(source);
        assert_eq!(0.5, vm.stack[2]);
        let source = vec![OP::Swap2, OP::PushNum(1.0), OP::Swap2];
        vm.run(source);
        assert_eq!(2.0, vm.stack[3]);
    }
    #[test]
    fn test_bring_set() {
        // performs : a = 1.0; b = 2.0; c = a - b; b = c;
        let source = vec![
            OP::PushNum(1.0),
            OP::PushNum(2.0),
            OP::Bring(0),
            OP::Bring(1),
            OP::Neg,
            OP::Add,
            OP::Bring(2),
            OP::Set(1),
        ];
        let mut stdout = io::stdout();
        let mut vm = Vm::new(&mut stdout);
        vm.run(source);
        assert_eq!(vec![1.0, -1.0, -1.0], vm.stack);
    }
    // executes the following :
    // a = num[3]
    // a[0] = 1
    // a[1] = 2
    // a[2] = 3
    #[test]
    fn test_heap() {
        let source = vec![
            // a = num[]
            OP::PushNum(3.0),
            OP::AllocObj(IS_SLICE_BIT + 3), // type : is_slice + size = 3
            // a[0] = 1
            OP::PushNum(1.0), // =1
            OP::Bring(0),     // a
            OP::PushNum(0.0), // [0]
            OP::Add,
            OP::SetHeap,
            // a[1] = 2
            OP::PushNum(2.0), // =2
            OP::Bring(0),     // a
            OP::PushNum(1.0), // [1]
            OP::Add,
            OP::SetHeap,
            // a[2] = 3
            OP::PushNum(3.0), // =3
            OP::Bring(0),     // a
            OP::PushNum(2.0), // [2]
            OP::Add,
            OP::SetHeap,
        ];
        let mut stdout = io::stdout();
        let mut vm = Vm::new(&mut stdout);
        vm.run(source);
        println!("heap : {:?}", vm.allocator.heap());
        println!("stack  : {:?}", vm.stack);
        assert_eq!(
            vm.allocator.heap(),
            vec![IS_SLICE_BIT + 3, 1, 2, 3, 0, allocator::MAX_HEAP_SIZE]
        );
    }
    //executes the following :
    // a = obj(size=1)
    // a.0 = obj(size=2)
    // a.0.0 = 7
    #[test]
    fn test_heap_deref_double() {
        let source = vec![
            // a = obj(size=1)
            OP::PushNum(1.0),
            OP::AllocObj(1 + 2), // type : 3 because 1 pointer and size 1 (1+2)
            // a.0 = obj(size=2)
            OP::PushNum(2.0), // = obj(size=2)
            OP::AllocObj(4),  // 4 because size 2
            OP::Bring(0),     // a
            OP::PushNum(0.0), // .0
            OP::Add,
            OP::SetHeap,
            // a.0.0 = 1
            OP::PushNum(7.0), // =1
            OP::Bring(0),     // a
            OP::PushNum(0.0), // .0
            OP::Add,
            OP::GetHeap,      //
            OP::PushNum(0.0), // .0
            OP::Add,
            OP::SetHeap,
        ];
        let mut stdout = io::stdout();
        let mut vm = Vm::new(&mut stdout);
        vm.run(source);
        println!("heap : {:?}", vm.allocator.heap());
        assert_eq!(
            vm.allocator.heap(),
            vec![3, 3, 4, 7, 0, 0, allocator::MAX_HEAP_SIZE]
        );
    }
}
