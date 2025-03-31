#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
use crate::database::Stores;
use crate::keys::{DbRef, Str};
use crate::state;
use crate::state::{Call, State};

pub const FUNCTIONS: &[(&str, Call)] = &[
    ("character", character),
    ("assert", assert),
    ("env_variables", env_variables),
    ("env_variable", env_variable),
    ("_tp_text_starts_with", _tp_text_starts_with),
    ("_tp_text_ends_with", _tp_text_ends_with),
    ("_tp_text_trim", _tp_text_trim),
    ("_tp_text_trim_start", _tp_text_trim_start),
    ("_tp_text_trim_end", _tp_text_trim_end),
    ("_tp_text_find", _tp_text_find),
    ("_tp_text_contains", _tp_text_contains),
    ("_tp_text_replace", _tp_text_replace),
    ("_tp_text_to_lowercase", _tp_text_to_lowercase),
    ("_tp_text_to_uppercase", _tp_text_to_uppercase),
    ("_tp_text_is_lowercase", _tp_text_is_lowercase),
    ("_tp_text_is_uppercase", _tp_text_is_uppercase),
    ("_tp_text_is_numeric", _tp_text_is_numeric),
    ("_tp_text_is_alphanumeric", _tp_text_is_alphanumeric),
    ("_tp_text_is_alphabetic", _tp_text_is_alphabetic),
    ("_tp_text_is_whitespace", _tp_text_is_whitespace),
    ("_tp_text_is_control", _tp_text_is_control),
    ("arguments", arguments),
    ("directory", directory),
    ("user_directory", user_directory),
    ("program_directory", program_directory),
];

pub fn init(state: &mut State) {
    for (name, implement) in FUNCTIONS {
        state.static_fn(name, *implement);
    }
}

fn character(stores: &mut Stores, stack: &mut DbRef) {
    let v_val = *stores.get::<Str>(stack);
    let new_value = { state::get_character(v_val.str()) };
    stores.put(stack, new_value);
}

fn assert(stores: &mut Stores, stack: &mut DbRef) {
    let v_message = *stores.get::<Str>(stack);
    let v_test = *stores.get::<bool>(stack);
    assert!(v_test, "{}", v_message.str());
}

fn env_variables(stores: &mut Stores, stack: &mut DbRef) {
    let new_value = { stores.os_variables() };
    stores.put(stack, new_value);
}

fn env_variable(stores: &mut Stores, stack: &mut DbRef) {
    let v_name = *stores.get::<Str>(stack);
    let new_value = { Stores::os_variable(v_name.str()) };
    stores.put(stack, new_value);
}

fn _tp_text_starts_with(stores: &mut Stores, stack: &mut DbRef) {
    let v_value = *stores.get::<Str>(stack);
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().starts_with(v_value.str()) };
    stores.put(stack, new_value);
}

fn _tp_text_ends_with(stores: &mut Stores, stack: &mut DbRef) {
    let v_value = *stores.get::<Str>(stack);
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().ends_with(v_value.str()) };
    stores.put(stack, new_value);
}

fn _tp_text_trim(stores: &mut Stores, stack: &mut DbRef) {
    let v_both = *stores.get::<Str>(stack);
    let new_value = { v_both.str().trim() };
    stores.put(stack, new_value);
}

fn _tp_text_trim_start(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().trim_start() };
    stores.put(stack, new_value);
}

fn _tp_text_trim_end(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().trim_end() };
    stores.put(stack, new_value);
}

fn _tp_text_find(stores: &mut Stores, stack: &mut DbRef) {
    let v_value = *stores.get::<Str>(stack);
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        if let Some(v) = v_self.str().find(v_value.str()) {
            v as i32
        } else {
            i32::MIN
        }
    };
    stores.put(stack, new_value);
}

fn _tp_text_contains(stores: &mut Stores, stack: &mut DbRef) {
    let v_value = *stores.get::<Str>(stack);
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().contains(v_value.str()) };
    stores.put(stack, new_value);
}

fn _tp_text_replace(stores: &mut Stores, stack: &mut DbRef) {
    let v_with = *stores.get::<Str>(stack);
    let v_value = *stores.get::<Str>(stack);
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().replace(v_value.str(), v_with.str()) };
    stores.put(stack, new_value);
}

fn _tp_text_to_lowercase(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().to_lowercase() };
    stores.put(stack, new_value);
}

fn _tp_text_to_uppercase(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = { v_self.str().to_uppercase() };
    stores.put(stack, new_value);
}

fn _tp_text_is_lowercase(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_lowercase() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn _tp_text_is_uppercase(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_uppercase() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn _tp_text_is_numeric(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_numeric() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn _tp_text_is_alphanumeric(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_alphanumeric() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn _tp_text_is_alphabetic(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_alphabetic() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn _tp_text_is_whitespace(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_whitespace() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn _tp_text_is_control(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        let mut res = true;
        for c in v_self.str().chars() {
            if !c.is_control() {
                res = false;
            }
        }
        res
    };
    stores.put(stack, new_value);
}

fn arguments(stores: &mut Stores, stack: &mut DbRef) {
    let new_value = { stores.os_arguments() };
    stores.put(stack, new_value);
}

fn directory(stores: &mut Stores, stack: &mut DbRef) {
    let v_v = *stores.get::<DbRef>(stack);
    let v_v = stores.store_mut(&v_v).addr_mut::<String>(v_v.rec, v_v.pos);
    let new_value = { Stores::os_directory(v_v) };
    stores.put(stack, new_value);
}

fn user_directory(stores: &mut Stores, stack: &mut DbRef) {
    let v_v = *stores.get::<DbRef>(stack);
    let v_v = stores.store_mut(&v_v).addr_mut::<String>(v_v.rec, v_v.pos);
    let new_value = { Stores::os_home(v_v) };
    stores.put(stack, new_value);
}

fn program_directory(stores: &mut Stores, stack: &mut DbRef) {
    let v_v = *stores.get::<DbRef>(stack);
    let v_v = stores.store_mut(&v_v).addr_mut::<String>(v_v.rec, v_v.pos);
    let new_value = { Stores::os_executable(v_v) };
    stores.put(stack, new_value);
}
