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

#Example

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

