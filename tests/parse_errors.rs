// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

mod testing;

#[test]
fn wrong_parameter() {
    code!("fn def(i: integer) { }\nfn test() { def(true); }")
        .error("Error: boolean should be integer on call to def at wrong_parameter:2:22")
        .warning("Parameter i is never read in wrong_parameter line 1:21");
}

#[test]
fn wrong_int() {
    code!("fn def(i: integer) {}\nenum EType {Eval}\nfn test() { def(Eval); }")
        .error("Error: EType should be integer on call to def at wrong_int:3:22")
        .warning("Parameter i is never read in wrong_int line 1:21");
}

#[test]
fn wrong_boolean() {
    code!("enum EType{ Val }\nfn def(t: EType) {}\nfn test() { def(true); }")
        .error("Error: boolean should be EType on call to def at wrong_boolean:3:22")
        .warning("Parameter t is never read in wrong_boolean line 2:19");
}

#[test]
fn unknown_var() {
    code!("fn test() { a == 1 }").error("Error: Unknown variable a at unknown_var:1:19");
}

#[test]
fn wrong_text() {
    code!("fn rout(a: integer) -> integer {if a > 4 {return \"a\"} 2}\nfn test() {}")
        .error("Error: text should be integer on return at wrong_text:1:53");
}

#[test]
fn empty_return() {
    code!("fn routine(a: integer) -> integer {if a > 4 {return} 1}\nfn test() {}")
        .error("Error: Expect expression after return at empty_return:1:53");
}

#[test]
fn wrong_void() {
    code!("fn rout(a: integer) {if a > 4 {return 12}}\nfn test() {}")
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
        .error("Error: Double attribute 'test.a' at double_field_name:1:35");
}

#[test]
fn incorrect_name() {
    code!("type something;\nfn something(a: integer) {}")
        .error("Error: Cannot redefine Type something at incorrect_name:2:27")
        .warning("Error: Expect type definitions to be in camel case style at incorrect_name:1:16");
}

#[test]
fn wrong_compare() {
    code!("enum EType{ V1 }\nenum Next{ V2 }\nfn test() { V1 == V2; }")
        .error("Error: No matching operator == on EType and Next at wrong_compare:3:21");
}

#[test]
fn wrong_plus() {
    code!("fn test() {(1 + \"a\")}")
        .error("Error: No matching operator + on integer and text at wrong_plus:1:20");
}

#[test]
fn wrong_if() {
    code!("fn test() {if 1 > 0 { 2 } else {\"a\"}\n}")
        .error("Error: text should be integer on if at wrong_if:2:1");
}

#[test]
fn wrong_assign() {
    code!("enum EType { V1 }\nfn test() {a = 1; a = V1 }")
        .error("Error: Cannot change type of variable a at wrong_assign:2:27");
}

#[test]
fn mixed_enums() {
    code!("enum E1 { V1 }\nenum E2 { V2 }\nfn a(v: E2) -> E2 { v }\nfn test() { a(V1) }")
        .error("Error: E1 should be E2 on call to a at mixed_enums:4:19");
}

#[test]
fn wrong_cast() {
    code!("enum E1 { V1 }\nfn test() { V1 as integer }")
        .error("Error: Unknown cast from E1 to integer at wrong_cast:2:28");
}

#[test]
fn undefined() {
    code!("fn test(v: V) -> V { v }").error("Error: Undefined type V at undefined:1:14");
}

#[test]
fn undefined_return() {
    code!("fn test(v: integer) -> V { v }")
        .error("Error: Undefined type V at undefined_return:1:27");
}

#[test]
fn undefined_as() {
    code!("fn test(v: integer) -> long { v as V }")
        .error("Error: Unknown cast from integer to V at undefined_as:1:39");
}

#[test]
fn undefined_enum() {
    code!("enum E1 { V1 }\nfn test(v: E1) -> boolean { v > V2 }")
        .error("Error: Unknown variable V2 at undefined_enum:2:37");
}

#[test]
fn unknown_sizeof() {
    code!("fn test() { sizeof(E); }")
        .error("Error: Expect a variable or type after sizeof at unknown_sizeof:1:22")
        .error("Error: Unknown variable E at unknown_sizeof:1:22");
}
