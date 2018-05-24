# Eptx

A interpreter for a programming language.

# Usage

Just create a file like the .ytp or .build


then run
```
cargo build --release
target/release/ytp my_file.ytp
```

currently it supports functions, strings and integers.

# Example

```
fn main(a) {
     var b = 5;
     while b < 50 {
        print("fac ", b, " is ", fac(b));
        b=b+1;
    }
}
fn fac(a){
    if a == 1{
        return 1;
    }
    return a * fac(a-1);
}
```

# Performance

The following becnhmarks were done on the following code : 
```
fn main(){
    // finds two factors of this number 

    num toFind = 75202;
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
| ytp (completely interpreted) | 53ms |
| ytp (using the vm) | 5ms |
| rust | 50us |
| python | 11ms |

The Vm interpreting the bytecode is way faster than the original interpreter.



# The virtual machine

The code is compiled to bytecode interpreted by a stack machine.

The machine has a few instructions (25 instructions) that manipulates the stack and can allocate memory on a heap.

# Garbage Collection

The garbage collection is not yet done but the memory layout already has the information to do so.

