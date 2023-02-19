// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

#[test]
fn wrong_parameter() {
    code!("fn def(i: integer) { }\nfn test() { def(true); }")
        .error("Error: Boolean should be Integer on call to def at wrong_parameter:2:22")
        .warning("Parameter i is never read in wrong_parameter line 1:8");
}

#[test]
fn wrong_int() {
    code!("fn def(i: integer) {}\nenum EType {Eval}\nfn test() { def(eval); }")
        .error("Error: EType should be Integer on call to def at wrong_int:3:22")
        .warning("Parameter i is never read in wrong_int line 1:8");
}

#[test]
fn wrong_boolean() {
    code!("enum EType{ Val }\nfn def(t: EType) {}\nfn test() { def(true); }")
        .error("Error: Type error Boolean should be EType on call to def at wrong_boolean:6:18")
        .warning("Parameter t is never read in wrong_boolean line 3:5");
}

#[test]
fn wrong_text() {
    code!("fn rout(a: integer) -> integer {if a > 4 {return \"a\"} 2}\nfn test() {}")
        .error("Error: Text should be Integer on return at wrong_text:1:53");
}

#[test]
fn empty_return() {
    code!("fn routine(a: integer) -> integer {if a > 4 {return} 1}\nfn test() {}")
        .error("Error: Expect expression after return at empty_return:1:53");
}

#[test]
fn wrong_void() {
    code!("fn rout(a: integer) {if a > 4 {return 12}}\nfn Test() {}")
        .error("Error: Expect no expression after return at wrong_void:1:42");
}

#[test]
fn wrong_break() {
    code!("fn test() {break}").error("Error: Cannot break outside a loop at wrong_break:1:18");
}

#[test]
fn wrong_continue() {
    code!("fn test() {continue}")
        .error("Error: Cannot continue outside a loop at wrong_continue:1:21");
}

#[test]
fn double_field_name() {
    code!("fn test(a: integer, b: integer, a: integer) { if a>b {} }")
        .error("Error: Double attribute 'test.a' at double_field_name:1:46");
}

#[test]
fn incorrect_name() {
    code!("type something;\nfn something(a: integer) {}")
        .error("Error: Cannot add fields or parameters to something at incorrect_name:2:27");
}

#[test]
fn wrong_compare() {
    code!("enum EType{ V1 }\nenum Next{ V2 }\nfn Test() { v1 == v2; }")
        .error("Error: No matching operator == on EType and Next at wrong_compare:3:21");
}

#[test]
fn wrong_plus() {
    code!("fn test() {(1 + \"a\")}")
        .error("Error: No matching operator + on Integer and Text at wrong_plus:1:20");
}

#[test]
fn wrong_if() {
    code!("fn test() {if 1 > 0 { 2 } else {\"a\"}\n}")
        .error("Error: Text should be Integer on if at wrong_if:2:1");
}
