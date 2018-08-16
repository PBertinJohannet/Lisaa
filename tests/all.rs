extern crate lisaa_lang;
use lisaa_lang::script::Script;
use std::fs::File;
use std::io::Read;

fn test_for(script: &str) {
    let (mut name, mut expect) = (
        "tests/scripts/test_".to_string(),
        "tests/scripts/expect_".to_string(),
    );
    name.push_str(script);
    name.push_str(".lisaa");
    expect.push_str(script);
    let mut expected_file = File::open(expect).expect(&format!(
        "could not open file tests/scripts/expect_{}",
        script
    ));
    let mut expected_result = String::new();
    expected_file
        .read_to_string(&mut expected_result)
        .expect("could not read file");
    expected_result = expected_result.replace("\r", ""); // ignore stupid windows crlf
    let result = Script::new(&name).run_program(expected_result.len());
    assert_eq!(result.expect("not an error"), expected_result);
}

/// Create a directory tests.
/// containing test_hunter2.rs and expect_hunter2
/// Add a test in tests.rs with the expec
#[test]
fn test_hello_world() {
    test_for("hello_world");
}

#[test]
fn test_char() {
    test_for("char");
}
#[test]
fn test_slice() {
    test_for("slice");
}
#[test]
fn test_num() {
    test_for("num");
}
#[test]
fn test_pi() {
    test_for("pi");
}
#[test]
fn test_fac() {
    test_for("fac");
}
#[test]
fn test_factors() {
    test_for("factors");
}
#[test]
fn test_overloading() {
    test_for("overloading");
}
#[test]
fn test_import() {
    test_for("import");
}
#[test]
fn test_generic_class() {
    test_for("generic_class");
}
#[test]
fn test_generic_func() {
    test_for("generic_func");
}
#[test]
fn test_nested_generics() {
    test_for("nested_generics");
}
