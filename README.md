# Eptx

A interpreter for a programming language.

# Usage

Just create a file like the .ytp or .build



```
cargo build --release
target/release/ytp my_file.ytp
```

It will parse and emit errors if some expressions could not be parsed correctly.
It will also evaluate unary operation.
