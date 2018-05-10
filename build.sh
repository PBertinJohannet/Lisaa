cargo build --release
RUST_BACKTRACE=2 ./target/release/ytp fail.ytp
