use std::mem::transmute;
/// How heap memory/GC works.
/// Everytime a pointer variable is created, an entry in root_references is created.
/// Everytime a heap object is created its reference is pushed to the top of the stack.
/// The first 64bits of the heap object is a reference to its type (usefull infos like size/refs...)
/// An allocator knows where the holes are in the heap and what size they are.
/// To do that there is a specific type with code 0 and the next byte is the allocated size
///  | 0 | 2 | . | here is a hole of size 2
/// Everytime the allocator needs to alloc it searches for a hole of the right size.
/// The GC is ran every ... allocations
/// The GC searches from the root refs and marks the unreachable objects. then it destroys them
/// and updates the holes in the allocator.
/// Currently, to avoid high fragmentation, the holes are filled only with holes of their size.

/// The allocator struct.
/// Allocates some memeory.
/// Allocates contigus memory.
/// The objects are stored in the following form :
/// | type of obj | first 64bits of obj | second 64bits | ...
/// When the GC removes memory, create a hole at the oject place :
/// | next hole adress | size of hole | .. | ...
#[derive(Debug)]
pub struct Allocator {
    first_hole : usize,
    heap : Vec<usize>,
}

impl Allocator {
    /// Creates a new empty allocator with its own heap.
    /// The allocator contains a hole of max size at its end.
    pub fn new() -> Self {
        Allocator {
            first_hole : 0,
            heap : vec![0, usize::max_value()],
        }
    }
    /// Allocate some memory of the required size.
    /// Returns the pointer to the allocated memory.
    /// The type must reference an actual type in the "struct_types" table.
    pub fn alloc(&mut self, size : usize, type_obj : usize) -> usize {
        let mut prev_hole = None;
        let mut next_hole = self.first_hole;
        while !(self.heap[next_hole+1] == size+1 || self.heap[next_hole+1] == usize::max_value()) {
            prev_hole = Some(next_hole);
            next_hole = self.heap[next_hole];
        }
        if self.heap[next_hole+1]==usize::max_value(){
            self.extend_heap(size+1 );
            if prev_hole.is_none(){
                self.first_hole += size+1;
            }
        }
        if let Some(prev) = prev_hole{
            self.connect_hole(prev, next_hole, size);
        } else {
            self.connect_first(next_hole, size);
        }
        self.fill_hole(next_hole, size, type_obj);
        return next_hole;
    }
    /// Frees an object in the heap.
    /// Creates a hole and link it.
    pub fn free(&mut self, position : usize, size : usize) {
        // If the first hole is at the end. (no fragmentation at all)
        self.heap[position] = self.first_hole;
        self.first_hole = position;
        self.heap[position+1] = size;
    }

    /// Fills a hole and update the remaining memory by creating a hole if necessary.
    /// No memory will remain.
    pub fn fill_hole(&mut self, hole : usize, size : usize, type_obj : usize){
        self.heap[hole] = type_obj;
    }

    /// Connects a hole to the future of a next hole..
    /// eg :
    /// A -> B -> C
    /// given A and B it connects A to C
    /// A -> C
    /// if b = usize::max_value() then
    /// A ->
    pub fn connect_hole(&mut self, prev : usize, next : usize, size_allocated : usize){
        if self.heap[next+1] == usize::max_value() {
            self.heap[prev] = next + size_allocated+1;
        } else {
            self.heap[prev] = self.heap[next];
        }
    }
    /// Connects the first hole to the future of a next hole..
    /// eg :
    /// A -> B -> C
    /// given A and B it connects A to C
    /// A -> C
    /// if b = usize::max_value() then
    /// A ->
    pub fn connect_first(&mut self, next : usize, size_allocated : usize){
        if self.heap[next+1] == usize::max_value() {
            self.first_hole = next + size_allocated+1;
        } else {
            self.first_hole = self.heap[next];
        }
    }
    /// Sets the given pointer at the given adress
    pub fn extend_heap(&mut self, size : usize) {
        for i in 0..size {
            self.heap.push(0);
        }
        let len = self.heap.len()-1;
        self.heap[len] = self.heap[len-size];
        self.heap[len-1] = self.heap[len-1-size];
    }

    /// Sets the given pointer at the given adress
    pub fn set_ptr(&mut self, adress : usize, value : usize) {
        self.heap[adress] = value;
    }
}


#[derive(Debug)]
enum OP {
    Inv,
    Mul,
    Swap2,
    Swap(usize),
    Bring(usize),
    Set(usize),
    Neg,
    Add,
    /// allocate an object of known size to the heap and put its adress at the top of the stack
    AllocObj,
    /// Access the value in the heap and push it to the stack.
    GetHeap,
    /// Take the top of the stack as an adress to the heap and set its value to the second value in
    /// the  stack. eg -> if the stack is 3, 2 the object at adress 2 will take value 3.
    SetHeap,
    /// Prints the value at the top of the stack as a char
    PrintStr,
    PushNum(f64),
    PrintNum,
}

/// no memory safety, everything leaks.
/// Stack contains only f64. everything else in the heap.
#[derive(Debug)]
pub struct Vm{
    /// The root references into the heap. from the stack/global
    root_references : Vec<usize>,
    //program : Vec<Vec<OP>>, matches functions to OPs.
    /// The stack contains ints/references in 64bit format.
    stack : Vec<f64>,
    allocator : Allocator,
}

impl Vm {
    pub fn new() -> Vm{
        Vm{
            root_references : vec![],
            stack : vec![],
            allocator : Allocator::new(),
        }
    }

    pub fn run(&mut self, program : Vec<OP>) {
        for op in program{
            println!("stack state : {:?}\nOP : {:?}\n\n", self, op);
            match op {
                OP::PushNum(n) => {
                    self.stack.push(n)
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
                OP::AllocObj => {
                    let size = self.stack.pop().unwrap() as i32 as usize;
                    let val = self.allocator.alloc(size, 1);
                    self.stack.push(val as f64);
                }
                OP::GetHeap => {
                    let adress : usize= self.stack.pop().unwrap() as usize;
                    let val : f64 = self.allocator.heap[adress] as f64;
                    self.stack.push(val);
                }
                OP::SetHeap => {
                    let (adress, value) = (self.stack.pop().unwrap() as usize,
                                                self.stack.pop().unwrap() as usize);
                    println!("val at : {} to {}", adress, value);
                    self.allocator.set_ptr(adress, value);
                }
                _ => panic!("unsupported operand"),
            }
        }
    }
}


#[cfg(test)]
mod tests_vm {
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
    fn test_bring_set(){
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
    // executes the following :
    // a = num[3]
    // a[0] = 1
    // a[1] = 2
    // a[2] = 3
    #[test]
    fn test_heap(){
        let mut source = vec![
        // a = num[]
            OP::PushNum(3.0),
            OP::AllocObj, // a is at pos 0
            // a[0] = 1
            OP::PushNum(1.0), // =1
            OP::Bring(0),   // a
            OP::PushNum(1.0), // [0]
            OP::Add,
            OP::SetHeap,
            // a[1] = 2
            OP::PushNum(2.0), // =2
            OP::Bring(0),   // a
            OP::PushNum(2.0), // [1]
            OP::Add,
            OP::SetHeap,
            // a[2] = 3
            OP::PushNum(3.0), // =3
            OP::Bring(0),   // a
            OP::PushNum(3.0), // [2]
            OP::Add,
            OP::SetHeap,
        ];
        let mut vm = Vm::new();
        vm.run(source);
        println!("heap : {:?}", vm.allocator.heap);
        assert_eq!(vm.allocator.heap, vec![1, 1, 2, 3, 0, usize::max_value()]);
    }
    //executes the following :
    // a = obj(size=1)
    // a.0 = obj(size=2)
    // a.0.0 = 7
    #[test]
    fn test_heap_deref_double(){
        let mut source = vec![
        // a = obj(size=1)
            OP::PushNum(1.0),
            OP::AllocObj, // a is at pos 0
            // a.0 = obj(size=2)
            OP::PushNum(2.0), // = obj(size=2)
            OP::AllocObj,
            OP::Bring(0),   // a
            OP::PushNum(1.0), // .0
            OP::Add,
            OP::SetHeap,
            // a.0.0 = 1
            OP::PushNum(7.0), // =1
            OP::Bring(0),   // a
            OP::PushNum(1.0), // .0
            OP::Add,
            OP::GetHeap,      //
            OP::PushNum(1.0), // .0
            OP::Add,
            OP::SetHeap,
        ];
        let mut vm = Vm::new();
        vm.run(source);
        println!("heap : {:?}", vm.allocator.heap);
        assert_eq!(vm.allocator.heap, vec![1, 2, 1, 7, 0, 0, usize::max_value()]);
    }
}

#[cfg(test)]
mod tests_allocator {
    use super::*;
    #[test]
    fn test_alloc_extend(){
        let mut alloc = Allocator::new();
        assert_eq!(alloc.heap, vec![0, usize::max_value()]);
        alloc.alloc(3, 1);
                                    // size 3 obj | empty | empy | empty | hole | hole size |
        assert_eq!(alloc.heap, vec![1, usize::max_value(), 0, 0, 0, usize::max_value()]);
        assert_eq!(alloc.first_hole, 4);
        alloc.alloc(1, 2);
        assert_eq!(alloc.heap, vec![1, usize::max_value(), 0, 0, 2, usize::max_value(), 0, usize::max_value()]);
        assert_eq!(alloc.first_hole, 6);
        alloc.alloc(5, 3);
        assert_eq!(alloc.heap, vec![1, usize::max_value(), 0, 0, 2, usize::max_value(),
                                    3, usize::max_value(),0,0,0,0,0,usize::max_value()]);
        assert_eq!(alloc.first_hole, 12);
    }

    #[test]
    fn test_free(){
        let mut alloc = Allocator::new();
        let first = alloc.alloc(3, 1);
        let sec = alloc.alloc(2, 1);
        println!("first : {}", first);
        alloc.free(first, 3+1);
        assert_eq!(alloc.heap, vec![7, 4, 0, 0, 1, usize::max_value(), 0, 0, usize::max_value()]);
        alloc.free(sec, 2+1);
        assert_eq!(alloc.heap, vec![7, 4, 0, 0, 0, 3, 0, 0, usize::max_value()]);
        assert_eq!(alloc.first_hole, 4);
    }

    #[test]
    fn test_alloc_after_free(){
        let mut alloc = Allocator::new();
        let first = alloc.alloc(3, 1);
        let sec = alloc.alloc(2, 1);
        alloc.free(first,3+1);
        println!("fail here \n\n{}\n{:?}\n\n",alloc.first_hole, alloc.heap);
        alloc.alloc(5, 2);
        assert_eq!(alloc.heap, vec![13, 4, 0, 0, 1, usize::max_value(), 0, 2, usize::max_value(),
                                    0, 0, 0, 0, 0, usize::max_value()]);
    }

    #[test]
    fn test_alloc_fil_first_hole(){
        let mut alloc = Allocator::new();
        let first = alloc.alloc(3, 1);
        let sec = alloc.alloc(2, 1);
        alloc.free(first,3+1);
        println!("fail here \n\n{}\n{:?}\n\n",alloc.first_hole, alloc.heap);
        alloc.alloc(3, 2);
        println!("first hole : {}", alloc.first_hole);
        assert_eq!(alloc.heap, vec![2, 4, 0, 0, 1, usize::max_value(), 0, 0, usize::max_value()]);
    }

    #[test]
    fn test_alloc_fil_second_hole(){
        let mut alloc = Allocator::new();
        let first = alloc.alloc(2, 1);
        let sec = alloc.alloc(3, 1);
        alloc.free(sec,3+1);
        alloc.free(first,2+1);
        println!("fail here \n\n{}\n{:?}\n\n",alloc.first_hole, alloc.heap);
        println!("first hole : {}", alloc.first_hole);
        // first is before the 3. link to 0
        assert_eq!(alloc.heap, vec![3, 3, 0, 7, 4, 0, 0, 0, usize::max_value()]);
        alloc.alloc(3, 2);
        assert_eq!(alloc.heap, vec![7, 3, 0, 2, 4, 0, 0, 0, usize::max_value()]);
    }
}
