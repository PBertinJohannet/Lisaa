//! The module for garbage collection.
//!
//! This is a work in progress
//!
//!
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
/// S
/// So how it works.
/// We need a table of objects types -> references.
/// We also need a table of root references.
/// How do we do for root references ?
/// Maybe when allocobj is called we add one.
/// then we need to keep track of all its copy/modifications etc...
/// Or !
/// When it is created we store it.
/// If it is no longer in the stack we destroy the reference.
/// But adding one will create a new ref without us noticing...
/// How do we do... :(
/// Maybe when one is created we keep a mark on where the stack was.
/// Then when the stack pass under we
/// Holy shit this is hard
/// Only remaining possibility is to add code everytime a ref is lost/added ?
/// Fuuu that is hard...
/// We can keep track of how many object are created and how many references are kept/modified.
/// eg :
///
///
/// So there
/// When we create an object, we add it to the list of allocated objects in the GC.
/// We also add it to the list of root references in the vm.
/// When it goes out of scope/its reference in the stack gets reassigned we remove it from the rootrefs
/// When we need to garbage collect... We start with the root refs and we traverse the graph of objects
/// and mark every object that can be reached.
///
/// Oh yea
/// Those fucking root refs...
/// Have a list of ref positions in the stack (easy this one).
/// Or is it easy
/// Imagine we alloc -> copy push1 add set
/// We have two refs in the stack.
/// Because of the copy.
/// But is there a way for the copy to find itself somewhere else
/// yes
/// push alloc add puts the alloc down.
///
/// So maybe create a root ref when allocated.
/// When destroyed
/// a[i] = Point -> call a[i] destructor -> removes it from the root refs.
///
/// Then we need to know when objects get out of scope.
///
/// Assume the list of holes goes from left to right, we create a map of all objects and mark them as unreachable.
/// Then we go from the root refs and mark all encountered objects.
/// Then we mark all the objects that were not
///
/// the bit 64, if set, says that this is a slice.
/// the other bits are indexes of the pointer's functions.
struct Ok{

}