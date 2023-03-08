// Copyright (c) 2022 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Testing framework
extern crate webassembly;

use webassembly::store::{Store};
//use webassembly::external::*;

enum Main {
    FItems = 4, // index of items on name/type (ref u32)
    FAges = 8, // persons on age (ref u32)
    FPositions = 12, // persons or places in radix (ref u32)
    FQuick = 16, // item names in hash (ref u32)
    Size = 20
}

enum Relation { // inside an array (need to be multiple of 4?)
    FRef = 0, // reference Person or Group
    FType = 4, // enum (u8) Parent, Child, Leader, Member, Teacher, Friend, Married, Sibling, Enemy
    Size = 5
}

enum History {
    FMoment = 0, // int can also be a future plan
    FOther = 4, // reference item
    FAction = 8, // enum Started, Ended, Created, Destroyed, Stayed
    Size = 9
}

enum ItemType {
    Person,
    Artefact,
    Group,
    Place
}

enum Person {
    Type = 4, // enum (u32) always the first field
    FName = 8, // text
    FHistory = 12, // ordered array
    FX = 16, // single f32
    FY = 20, // single f32
    FRelations = 24, // relations
    FAge = 28, // integer
    FSex = 32, // enum Male/Female/Non-binary
    Size = 33
}

enum Artefact {
    Type = 4, // enum (u32) always the first field
    FName = 8, // text
    FHistory = 12, // ordered array
    FX = 16, // single f32
    FY = 20, // single f32
    FRelations = 24, // relations
    FWeight = 28, // integer u8
    FBulk = 29, // integer u8
    Size = 30
}

enum Group {
    Type = 4, // enum (u32) always the first field
    FName = 8, // text
    FHistory = 12, // ordered array
    FFormal = 16, // boolean
    FRelations = 24, // relations
    Size = 28
}

enum Place {
    Type = 4, // enum (u32)
    FName = 8, // text
    FHistory = 12, // ordered array
    FX = 16, // single f32
    FY = 20, // single f32
    FTerrain = 24, // enum (ref u8)
    FRoughness = 25, // integer (u8)
    Size = 26
}

#[test]
fn object() {
    let mut st = structure();
    person(&mut st, "Eric", 32, Sex::Male);
    person(&mut st, "Liz", 30, Sex::Female);
    person(&mut st, "Debra", 8, Sex::Female);
    place(&mut st, "Elmshill", (100.0, 100.0));
    // write out structure as json
    // fill structure with some json
    // read json again in a different structure
    // mutate both structures a bit in different order
    // write that out too and compare results
}

fn structure() -> Store {
    let mut s = Store::new(1000);
    let r = s.claim(Main::Size as u32);
    s.set_int(r, Main::FItems as isize, 0);
    s.set_int(r, Main::FAges as isize, 0);
    s.set_int(r, Main::FPositions as isize, 0);
    s.set_int(r, Main::FQuick as isize, 0);
    s
}

enum Sex {
    Male,
    Female,
    NonBinary,
}

fn person(s: &mut Store, name: &str, age: i32, sex: Sex) -> u32 {
    let r = s.claim(Person::Size as u32);
    s.set_int(r, Person::Type as isize, ItemType::Person as i32);
    let name_ref = s.set_str(name);
    s.set_int(r, Person::FName as isize, name_ref);
    s.set_int(r, Person::FHistory as isize, 0);
    s.set_single(r, Person::FX as isize, 0.0);
    s.set_single(r, Person::FY as isize, 0.0);
    s.set_int(r, Person::FRelations as isize, 0);
    s.set_int(r, Person::FAge as isize, age);
    s.set_byte(r, Person::FAge as isize, 0, sex as i32);
    r
}

fn artefact(s: &mut Store, name: &str, weight: i32, bulk: u8) -> u32 {
    let r = s.claim(Artefact::Size as u32);
    s.set_int(r, Artefact::Type as isize, ItemType::Artefact as i32);
    let name_ref = s.set_str(name);
    s.set_int(r, Artefact::FName as isize, name_ref);
    s.set_int(r, Artefact::FHistory as isize, 0);
    s.set_single(r, Artefact::FX as isize, 0.0);
    s.set_single(r, Artefact::FY as isize, 0.0);
    s.set_int(r, Artefact::FRelations as isize, 0);
    s.set_int(r, Artefact::FWeight as isize, weight);
    s.set_byte(r, Artefact::FBulk as isize, 0, bulk as i32);
    r
}

fn group(s: &mut Store, name: &str) -> u32 {
    let r = s.claim(Group::Size as u32);
    s.set_int(r, Group::Type as isize, ItemType::Group as i32);
    let name_ref = s.set_str(name);
    s.set_int(r, Group::FName as isize, name_ref);
    s.set_int(r, Group::FHistory as isize, 0);
    s.set_single(r, Group::FHistory as isize, 0.0);
    s.set_single(r, Group::FFormal as isize, 0.0);
    s.set_int(r, Group::FRelations as isize, 0);
    r
}

fn place(s: &mut Store, name: &str, p: (f32, f32)) -> u32 {
    let r = s.claim(Place::Size as u32);
    s.set_int(r, Place::Type as isize, ItemType::Place as i32);
    let name_ref = s.set_str(name);
    s.set_int(r, Place::FName as isize, name_ref);
    s.set_int(r, Place::FHistory as isize, 0);
    s.set_single(r, Place::FX as isize, p.0);
    s.set_single(r, Place::FY as isize, p.1);
    s.set_byte(r, Place::FTerrain as isize, 0, 0);
    s.set_byte(r, Place::FTerrain as isize, 0, 0);
    r
}
