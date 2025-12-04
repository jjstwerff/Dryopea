#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
use crate::database::Stores;
use crate::keys::{DbRef, Str};
use crate::state::{Call, State};

pub const FUNCTIONS: &[(&str, Call)] = &[
    ("n_assert", assert),
    ("n_panic", panic),
    ("n_env_variables", env_variables),
    ("n_env_variable", env_variable),
    ("t_4text_starts_with", _tp_text_starts_with),
    ("t_4text_ends_with", _tp_text_ends_with),
    ("t_4text_trim", _tp_text_trim),
    ("t_4text_trim_start", _tp_text_trim_start),
    ("t_4text_trim_end", _tp_text_trim_end),
    ("t_4text_find", _tp_text_find),
    ("t_4text_rfind", _tp_text_rfind),
    ("t_4text_contains", _tp_text_contains),
    ("t_4text_replace", _tp_text_replace),
    ("t_4text_to_lowercase", _tp_text_to_lowercase),
    ("t_4text_to_uppercase", _tp_text_to_uppercase),
    ("t_4text_is_lowercase", _tp_text_is_lowercase),
    ("t_9character_is_lowercase", _tp_character_is_lowercase),
    ("t_4text_is_uppercase", _tp_text_is_uppercase),
    ("t_9character_is_uppercase", _tp_character_is_uppercase),
    ("t_4text_is_numeric", _tp_text_is_numeric),
    ("t_9character_is_numeric", _tp_character_is_numeric),
    ("t_4text_is_alphanumeric", _tp_text_is_alphanumeric),
    (
        "t_9character_is_alphanumeric",
        _tp_character_is_alphanumeric,
    ),
    ("t_5text_is_alphabetic", _tp_text_is_alphabetic),
    ("t_9character_is_alphabetic", _tp_character_is_alphabetic),
    ("t_4text_is_whitespace", _tp_text_is_whitespace),
    ("t_4text_is_control", _tp_text_is_control),
    ("n_arguments", arguments),
    ("n_directory", directory),
    ("n_user_directory", user_directory),
    ("n_program_directory", program_directory),
];

pub fn init(state: &mut State) {
    for (name, implement) in FUNCTIONS {
        state.static_fn(name, *implement);
    }
}

fn assert(stores: &mut Stores, stack: &mut DbRef) {
    let v_message = *stores.get::<Str>(stack);
    let v_test = *stores.get::<bool>(stack);
    assert!(v_test, "{}", v_message.str());
}

fn panic(stores: &mut Stores, stack: &mut DbRef) {
    let v_message = *stores.get::<Str>(stack);
    panic!("{}", v_message.str());
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

fn _tp_text_rfind(stores: &mut Stores, stack: &mut DbRef) {
    let v_value = *stores.get::<Str>(stack);
    let v_self = *stores.get::<Str>(stack);
    let new_value = {
        if let Some(v) = v_self.str().rfind(v_value.str()) {
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

fn _tp_character_is_lowercase(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<char>(stack);
    let new_value = { v_self.is_lowercase() };
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

fn _tp_character_is_uppercase(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<char>(stack);
    let new_value = { v_self.is_uppercase() };
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

fn _tp_character_is_numeric(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<char>(stack);
    let new_value = { v_self.is_numeric() };
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

fn _tp_character_is_alphanumeric(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<char>(stack);
    let new_value = { v_self.is_alphanumeric() };
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

fn _tp_character_is_alphabetic(stores: &mut Stores, stack: &mut DbRef) {
    let v_self = *stores.get::<char>(stack);
    let new_value = { v_self.is_alphabetic() };
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
