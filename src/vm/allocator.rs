
/// How heap memory/GC works.
/// Everytime a pointer variable is created, an entry in root_references is created.
/// Everytime a heap object is created its reference is pushed to the top of the stack.
/// The first 64bits of the heap object is a reference to its type (usefull infos like size/refs...)
/// An allocator knows where the holes are in the heap and what size they are.
/// To do that there is a specific type with code 0 and the next byte is the allocated size
///  | 0 | 2 | . | here is a hole of size 2
/// Everytime the allocator needs to alloc it searches for a hole of the right size.
/// The GC is ran every ... allocations
/// when the gc runs it takes all allocated objects and mark them.
/// The GC searches from the root refs and unmarks the unreachable objects. then it destroys them
/// and updates the holes in the allocator.
/// Currently, to avoid high fragmentation, the holes are filled only with objects of their size.
///
/// So how to mark slices... -> (first bit at 1;  61 next bits =size ;2 last bits object type Pointer/char/int)
/// We mark other objects then.
/// The object info is given to the vm through a list of objectinfos.
///
/// The allocator struct.
/// Allocates some memory.
/// Allocates contigus memory.
/// The objects are stored in the following form :
/// | type of obj | first 64bits of obj | second 64bits | ...
/// When the GC removes memory, create a hole at the oject place :
/// | next hole adress | size of hole | .. | ...
///
///
/// !!! !! !!!! ! Important (or not) the heap cannot take more than 2**52 bytes in memory. maybe
/// this will be important in 2030
///
use std::mem::transmute;
pub const MAX_HEAP_SIZE : f64 = 4503599627370496.0;//2.0f64.powf(52.0);

#[derive(Debug)]
pub struct Allocator {
    first_hole: f64,
    heap: Vec<f64>,
}

impl Allocator {
    /// Creates a new empty allocator with its own heap.
    /// The allocator contains a hole of max size at its end.
    pub fn new() -> Self {
        Allocator {
            first_hole: 0.0,
            heap: vec![0.0, MAX_HEAP_SIZE],
        }
    }
    /// Allocate some memory of the required size.
    /// Returns the pointer to the allocated memory.
    /// The type must reference an actual type in the "struct_types" table.
    /// Returns the reference to the start of the object's value (not its type).
    pub fn alloc(&mut self, size: usize, type_obj: usize) -> usize {
        if size == 0 {
            return 0;
        }
        let mut prev_hole = None;
        let mut next_hole = self.first_hole;
        while !(self.heap[next_hole as usize + 1] == (size as f64 + 1.0)
            || self.heap[next_hole as usize + 1] == MAX_HEAP_SIZE)
            {
                prev_hole = Some(next_hole);
                next_hole = self.heap[next_hole as usize];
                println!("next hole {}", next_hole);
            }
        if self.heap[next_hole as usize + 1] == MAX_HEAP_SIZE {
            self.extend_heap(size + 1);
            if prev_hole.is_none() {
                self.first_hole += size as f64 + 1.0;
            }
        }
        if let Some(prev) = prev_hole {
            self.connect_hole(prev as usize, next_hole as usize, size);
        } else {
            self.connect_first(next_hole as usize, size);
        }
        self.fill_hole(next_hole as usize, type_obj);
        return next_hole as usize + 1;
    }
    /// Frees an object in the heap.
    /// Creates a hole and link it.
    #[allow(dead_code)]
    pub fn free(&mut self, position: usize, size: usize) {
        // If the first hole is at the end. (no fragmentation at all)
        self.heap[position-1] = self.first_hole as f64;
        self.first_hole = position as f64-1.0;
        self.heap[position] = size as f64;
    }

    /// Fills a hole and update the remaining memory by creating a hole if necessary.
    /// No memory will remain.
    pub fn fill_hole(&mut self, hole: usize, type_obj: usize) {
        self.heap[hole] = type_obj as f64;
    }

    /// Connects a hole to the future of a next hole..
    /// eg :
    /// A -> B -> C
    /// given A and B it connects A to C
    /// A -> C
    /// if b = usize::max_value() then
    /// A ->
    pub fn connect_hole(&mut self, prev: usize, next: usize, size_allocated: usize) {
        if self.heap[next + 1] == MAX_HEAP_SIZE {
            self.heap[prev] = next as f64 + size_allocated as f64 + 1.0;
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
    pub fn connect_first(&mut self, next: usize, size_allocated: usize) {
        if self.heap[next + 1] == MAX_HEAP_SIZE {
            self.first_hole = next as f64 + size_allocated as f64 + 1.0;
        } else {
            self.first_hole = self.heap[next];
        }
    }
    /// Sets the given pointer at the given adress
    pub fn extend_heap(&mut self, size: usize) {
        for _ in 0..size {
            self.heap.push(0.0);
        }
        let len = self.heap.len() - 1;
        self.heap[len] = self.heap[len - size];
        self.heap[len - 1] = self.heap[len - 1 - size];
    }

    /// Sets the given pointer at the given adress
    pub fn set_ptr(&mut self, adress: usize, value: f64) {
        if adress > self.heap.len() - 1 {
            println!("Segmentation fault (Core dumped)");
            panic!("Program exited");
        }
        self.heap[adress] = value;
    }

    /// Get value at address
    pub fn get_heap(&self, adress : usize) -> f64 {
        self.heap[adress]
    }
    /// returns the heap
    pub fn heap(&self)-> &Vec<f64>{
        &self.heap
    }
    /// given a vec of references to objects,
    /// Runs garbage collection
    pub fn run_gc(&mut self){

    }
}



#[cfg(test)]
mod tests_allocator {
    use super::*;
    #[test]
    fn test_alloc_extend() {
        let mut alloc = Allocator::new();
        assert_eq!(alloc.heap, vec![0.0, MAX_HEAP_SIZE]);
        alloc.alloc(3, 1);
        // size 3 obj | empty | empy | empty | hole | hole size |
        assert_eq!(
            alloc.heap,
            vec![1.0, MAX_HEAP_SIZE, 0.0, 0.0, 0.0, MAX_HEAP_SIZE]
        );
        assert_eq!(alloc.first_hole, 4.0);
        alloc.alloc(1, 2);
        assert_eq!(
            alloc.heap,
            vec![
                1.0,
                MAX_HEAP_SIZE,
                0.0,
                0.0,
                2.0,
                MAX_HEAP_SIZE,
                0.0,
                MAX_HEAP_SIZE,
            ]
        );
        assert_eq!(alloc.first_hole, 6.0);
        alloc.alloc(5, 3);
        assert_eq!(
            alloc.heap,
            vec![
                1.0,
                MAX_HEAP_SIZE,
                0.0,
                0.0,
                2.0,
                MAX_HEAP_SIZE,
                3.0,
                MAX_HEAP_SIZE,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                MAX_HEAP_SIZE,
            ]
        );
        assert_eq!(alloc.first_hole, 12.0);
    }

    #[test]
    fn test_free() {
        let mut alloc = Allocator::new();
        let first = alloc.alloc(3, 1);
        let sec = alloc.alloc(2, 1);
        println!("first : {}", first);
        alloc.free(first, 3 + 1);
        assert_eq!(
            alloc.heap,
            vec![7.0, 4.0, 0.0, 0.0, 1.0, MAX_HEAP_SIZE, 0.0, 0.0, MAX_HEAP_SIZE]
        );
        alloc.free(sec, 2 + 1);
        assert_eq!(alloc.heap, vec![7.0, 4.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, MAX_HEAP_SIZE]);
        assert_eq!(alloc.first_hole, 4.0);
    }

    #[test]
    fn test_alloc_after_free() {
        let mut alloc = Allocator::new();
        let first = alloc.alloc(3, 1);
        alloc.alloc(2, 1);
        alloc.free(first, 3 + 1);
        println!("fail here \n\n{}\n{:?}\n\n", alloc.first_hole, alloc.heap);
        alloc.alloc(5, 2);
        assert_eq!(
            alloc.heap,
            vec![
                13.0,
                4.0,
                0.0,
                0.0,
                1.0,
                MAX_HEAP_SIZE,
                0.0,
                2.0,
                MAX_HEAP_SIZE,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                MAX_HEAP_SIZE,
            ]
        );
    }

    #[test]
    fn test_alloc_fil_first_hole() {
        let mut alloc = Allocator::new();
        let first = alloc.alloc(3, 1);
        let _sec = alloc.alloc(2, 1);
        alloc.free(first, 3 + 1);
        println!("fail here \n\n{}\n{:?}\n\n", alloc.first_hole, alloc.heap);
        alloc.alloc(3, 2);
        println!("first hole : {}", alloc.first_hole);
        assert_eq!(
            alloc.heap,
            vec![2.0, 4.0, 0.0, 0.0, 1.0, MAX_HEAP_SIZE, 0.0, 0.0, MAX_HEAP_SIZE]
        );
    }

    #[test]
    fn test_alloc_fil_second_hole() {
        let mut alloc = Allocator::new();
        let first = alloc.alloc(2, 1);
        let sec = alloc.alloc(3, 1);
        alloc.free(sec, 3 + 1);
        alloc.free(first, 2 + 1);
        println!("fail here \n\n{}\n{:?}\n\n", alloc.first_hole, alloc.heap);
        println!("first hole : {}", alloc.first_hole);
        // first is before the 3. link to 0
        assert_eq!(alloc.heap, vec![3.0, 3.0, 0.0, 7.0, 4.0, 0.0, 0.0, 0.0, MAX_HEAP_SIZE]);
        alloc.alloc(3, 2);
        assert_eq!(alloc.heap, vec![7.0, 3.0, 0.0, 2.0, 4.0, 0.0, 0.0, 0.0, MAX_HEAP_SIZE]);
    }
}
