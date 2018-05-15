use std::mem::transmute;

enum OP {
    Inv,
    Mul,
    Swap2,
    Swap(usize),
    Bring(usize),
    Set(usize),
    Neg,
    Add,
    PushStr(String),
    PrintStr,
    PushNum(f64),
    PrintNum,
}

/// no memory safety, everything leaks.
/// Stack contains only f64. everything else in the heap.
#[derive(Debug)]
pub struct Vm{
    //program : Vec<Vec<OP>>, matches functions to OPs.
    /// The stack contains ints/references in 64bit format.
    stack : Vec<f64>,
    heap : Vec<i64>,
}

impl Vm {
    pub fn new() -> Vm{
        Vm{
            stack : vec![],
            heap : vec![],
        }
    }

    pub fn run(&mut self, program : Vec<OP>) {
        for op in program{
            match op {
                OP::PushNum(n) => {
                    self.stack.push(n)
                },
                OP::PushStr(s) => {
                    panic!("push str not implemented yet");
                    //for
                    //self.heap.push(s);
                    //self.stack.push(s)
                },
                OP::PrintNum => {
                    print!("{}", self.stack.pop().unwrap());
                }
                OP::Inv => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(1.0/val)
                }
                OP::Mul => {
                    let val = self.stack.pop().unwrap()*self.stack.pop().unwrap();
                    self.stack.push(val)
                }
                OP::Neg => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(-val)
                }
                OP::Add => {
                    let val = self.stack.pop().unwrap()+self.stack.pop().unwrap();
                    self.stack.push(val)
                }
                OP::Swap2 => {
                    let (high, low) = (self.stack.pop().unwrap(), self.stack.pop().unwrap());
                    self.stack.push(high);
                    self.stack.push(low);
                }
                OP::Swap(id) => {
                    let to_rem = self.stack.len()-id-1;
                    let val = self.stack.swap_remove(to_rem);
                    self.stack.push(val);
                }
                OP::Bring(id) => {
                    let val = self.stack[id];
                    self.stack.push(val);
                }
                OP::Set(id) => {
                    let to_set = self.stack.pop().unwrap();
                    self.stack[id] = to_set;
                }
                _ => panic!("unsupported operand"),
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_load(){
        let mut source = vec![OP::PushNum(1.0), OP::PrintNum];
        let mut vm = Vm::new();
        vm.run(source);
        assert_eq!(0, vm.stack.len());
        let mut source = vec![OP::PushNum(1.0), OP::PushNum(0.1)];
        vm.run(source);
        assert_eq!(2, vm.stack.len());
    }
    #[test]
    fn test_operations(){
        let mut source = vec![OP::PushNum(1.0), OP::PushNum(2.0), OP::Add, OP::Neg];
        let mut vm = Vm::new();
        vm.run(source);
        assert_eq!(1, vm.stack.len());
        assert_eq!(-3.0, vm.stack[0]);
        let mut source = vec![OP::PushNum(1.0), OP::Mul];
        vm.run(source);
        assert_eq!(-3.0, vm.stack[0]);
        let mut source = vec![OP::PushNum(1.0)];
        vm.run(source);
        assert_eq!(-3.0, vm.stack[0]);
    }
    #[test]
    fn test_swap(){
        let mut source = vec![OP::PushNum(1.0), OP::PushNum(2.0), OP::PushNum(0.5), OP::Swap2];
        let mut vm = Vm::new();
        vm.run(source);
        assert_eq!(3, vm.stack.len());
        assert_eq!(1.0, vm.stack[0]);
        assert_eq!(0.5, vm.stack[1]);
        let mut source = vec![OP::Swap(2)];
        vm.run(source);
        assert_eq!(2.0, vm.stack[0]);
        let mut source = vec![OP::PushNum(1.0), OP::Swap(3)];
        vm.run(source);
        assert_eq!(1.0, vm.stack[0]);
    }
    #[test]
    fn test_bring(){
        // performs : a = 1.0; b = 2.0; c = a - b; b = c;
        let mut source = vec![OP::PushNum(1.0),
                              OP::PushNum(2.0),
                              OP::Bring(0),
                              OP::Bring(1),
                              OP::Neg,
                              OP::Add,
                              OP::Bring(2),
                              OP::Set(1)];
        let mut vm = Vm::new();
        vm.run(source);
        assert_eq!(vec![1.0, -1.0, -1.0], vm.stack);
    }
}
