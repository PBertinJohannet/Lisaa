# Lisaa

Lisaa is surely an acronym

The lisaa language is a simple scripting language featuring :

* Static typing
* Traits
* Generic types
* Constrained generics
* function overloading
* modules

# Usage

Just create a file in .lisaa and pass it as first argument

```
cargo build --release
target/release/lisaa my_file.lisaa
```


# Example
This example demonstrates the features of the lisaa language : 
It defines trait PointInner containing two methods and Point class containing type variables constrained by the PointInner class. 
Then uses them in various ways.
```
import string

trait PointInner = method add(Self) -> Self + method toString() -> String;

Class Point<T : PointInner> {
    T x = empty::<T>();
    T y = empty::<T>();
}

fn NewPoint<T : PointInner>(T x, T y) -> Point<T> {
    Point<T> p = Point::<T>();
    p.x = x;
    p.y = y;
    return p;
}

method toString() -> String of Point<num> {
    return self.x.toString()+", "+self.y.toString();
}

method add<T : PointInner>(Point<T> other) -> Point<T> of Point<T> {
    Point<T> p = Point::<T>();
    p.x = self.x+other.x;
    p.y = self.y+other.y;
    return p;
}

method joinInners<U : PointInner>(String s) -> String of Point<U>{
    return self.x.toString()+s+self.y.toString();
}

fn sum<T : PointInner>(Point<T> p, T a, T b) -> Point<T> {
    p.x = p.x + a;
    p.y = p.y + b;
    return p;
}

fn main(){
    Point<num> p = NewPoint(2, 52);
    Point<num> c = NewPoint(25, -11);
    Point<Point<num>> big = NewPoint(p, c);
    sum(big, c, c).joinInners(" : ").println();
}
```

Look at the source of string.lisaa to find more.

# Performance

The following benchmarks were done on the following code :
```
fn main(){
    // finds two factors of this number (241 and 307) 

    num toFind = 73987;
    num a = 0;
    num b = 0;
    num found = 0;
    while !found && a < toFind/2{
        a = a+1;
        b = 0;
        while !found && b < toFind/2{
            b = b + 1;
            if a * b == toFind{
                found = 1;
            }
        }
    }
}
```

| Language/interpreter | time  |
| -------------------- | --------- |
| lisaa (ast based interpreter) | 6000 ms |
| python | 1269 ms |
| lisaa (using the vm) | 500 ms |
| java | 60 ms |
| rust | 7 ms |

The Vm interpreting the bytecode is way faster than the original interpreter.

# Original interpreter

The original ast-based interpreter is now deprecated, it was too slow.
All the codes will now be compiled to be interpreted by the virtual machine

# The virtual machine

The code is compiled to bytecode interpreted by a stack machine.

The machine has a few instructions that manipulates the stack and can allocate memory on a heap.

# Garbage Collection

No garbage collector at the moment, everything leaks.

