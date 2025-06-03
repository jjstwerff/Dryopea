#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_truncation)]
use crate::external;
use crate::keys::{DbRef, Str};
use crate::state::State;
use crate::vector;

pub const OPERATORS: &[fn(&mut State)] = &[
    goto,
    goto_word,
    goto_false,
    goto_false_word,
    stack_pos,
    call,
    op_return,
    free_stack,
    const_true,
    const_false,
    var_bool,
    put_bool,
    not,
    format_bool,
    const_int,
    const_short,
    const_tiny,
    var_int,
    put_int,
    conv_int_from_null,
    const_long_text,
    cast_int_from_text,
    cast_long_from_text,
    cast_single_from_text,
    cast_float_from_text,
    abs_int,
    min_single_int,
    conv_long_from_int,
    conv_float_from_int,
    conv_single_from_int,
    conv_bool_from_int,
    add_int,
    min_int,
    mul_int,
    div_int,
    rem_int,
    land_int,
    lor_int,
    eor_int,
    s_left_int,
    s_right_int,
    eq_int,
    ne_int,
    lt_int,
    le_int,
    gt_int,
    ge_int,
    format_int,
    const_long,
    var_long,
    put_long,
    conv_long_from_null,
    abs_long,
    min_single_long,
    cast_int_from_long,
    conv_float_from_long,
    conv_bool_from_long,
    add_long,
    min_long,
    mul_long,
    div_long,
    rem_long,
    land_long,
    lor_long,
    eor_long,
    s_left_long,
    s_right_long,
    eq_long,
    ne_long,
    lt_long,
    le_long,
    gt_long,
    ge_long,
    format_long,
    const_single,
    var_single,
    put_single,
    conv_single_from_null,
    abs_single,
    min_single_single,
    cast_int_from_single,
    cast_long_from_single,
    conv_float_from_single,
    conv_bool_from_single,
    add_single,
    min_single,
    mul_single,
    div_single,
    rem_single,
    math_cos_single,
    math_sin_single,
    math_tan_single,
    math_acos_single,
    math_asin_single,
    math_atan_single,
    math_atan2_single,
    math_ceil_single,
    math_floor_single,
    math_round_single,
    math_sqrt_single,
    math_log_single,
    pow_single,
    eq_single,
    ne_single,
    lt_single,
    le_single,
    gt_single,
    ge_single,
    format_single,
    const_float,
    var_float,
    put_float,
    conv_float_from_null,
    abs_float,
    math_pi_float,
    math_e_float,
    math_cos_float,
    math_sin_float,
    math_tan_float,
    math_acos_float,
    math_asin_float,
    math_atan_float,
    math_atan2_float,
    math_ceil_float,
    math_floor_float,
    math_round_float,
    math_sqrt_float,
    math_log_float,
    pow_float,
    min_single_float,
    cast_single_from_float,
    cast_int_from_float,
    cast_long_from_float,
    conv_bool_from_float,
    add_float,
    min_float,
    mul_float,
    div_float,
    rem_float,
    eq_float,
    ne_float,
    lt_float,
    le_float,
    gt_float,
    ge_float,
    format_float,
    var_text,
    arg_text,
    const_text,
    conv_text_from_null,
    length_text,
    conv_bool_from_text,
    add_text,
    text,
    append_text,
    get_text_sub,
    clear_text,
    free_text,
    eq_text,
    ne_text,
    lt_text,
    le_text,
    gt_text,
    ge_text,
    format_text,
    append_character,
    var_enum,
    const_enum,
    put_enum,
    conv_bool_from_enum,
    cast_text_from_enum,
    cast_enum_from_text,
    cast_int_from_enum,
    cast_enum_from_int,
    conv_enum_from_null,
    eq_enum,
    ne_enum,
    lt_enum,
    le_enum,
    gt_enum,
    ge_enum,
    database,
    format_database,
    conv_bool_from_ref,
    conv_ref_from_null,
    free_ref,
    append,
    var_ref,
    put_ref,
    eq_ref,
    ne_ref,
    get_ref,
    set_ref,
    get_field,
    get_int,
    get_long,
    get_single,
    get_float,
    get_byte,
    get_enum,
    set_enum,
    get_short,
    get_text,
    set_int,
    set_long,
    set_single,
    set_float,
    set_byte,
    set_short,
    set_text,
    var_vector,
    length_vector,
    clear_vector,
    get_vector,
    cast_vector_from_text,
    remove_vector,
    insert_vector,
    new_record,
    finish_record,
    append_vector,
    get_record,
    validate,
    hash_add,
    hash_find,
    hash_remove,
    eq_bool,
    ne_bool,
    panic,
    print,
    iterate,
    step,
    remove,
    append_copy,
    copy_record,
    static_call,
    create_ref,
    get_ref_text,
    get_db_ref,
    set_db_ref,
    append_ref_text,
    clear_ref_text,
    get_file,
    get_dir,
    get_png_image,
    get_file_text,
];

fn goto(s: &mut State) {
    let v_step = *s.code::<i8>();
    s.code_pos = (s.code_pos as i32 + i32::from(v_step)) as u32;
}

fn goto_word(s: &mut State) {
    let v_step = *s.code::<i16>();
    s.code_pos = (s.code_pos as i32 + i32::from(v_step)) as u32;
}

fn goto_false(s: &mut State) {
    let v_step = *s.code::<i8>();
    let v_if_false = *s.get_stack::<bool>();
    if !v_if_false {
        s.code_pos = (s.code_pos as i32 + i32::from(v_step)) as u32;
    }
}

fn goto_false_word(s: &mut State) {
    let v_step = *s.code::<i16>();
    let v_if_false = *s.get_stack::<bool>();
    if !v_if_false {
        s.code_pos = (s.code_pos as i32 + i32::from(v_step)) as u32;
    }
}

fn stack_pos(s: &mut State) {
    s.stack_pos();
}

fn call(s: &mut State) {
    let v_size = *s.code::<u16>();
    let v_to = *s.code::<i32>();
    s.fn_call(v_size, v_to);
}

fn op_return(s: &mut State) {
    let v_ret = *s.code::<u16>();
    let v_value = *s.code::<u8>();
    let v_discard = *s.code::<u16>();
    s.fn_return(v_ret, v_value, v_discard);
}

fn free_stack(s: &mut State) {
    let v_value = *s.code::<u8>();
    let v_discard = *s.code::<u16>();
    s.free_stack(v_value, v_discard);
}

fn const_true(s: &mut State) {
    let new_value = true;
    s.put_stack(new_value);
}

fn const_false(s: &mut State) {
    let new_value = false;
    s.put_stack(new_value);
}

fn var_bool(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<bool>(v_pos);
    s.put_stack(new_value);
}

fn put_bool(s: &mut State) {
    let v_var = *s.code::<u16>();
    let v_value = *s.get_stack::<bool>();
    s.put_var(v_var, v_value);
}

fn not(s: &mut State) {
    let v_v1 = *s.get_stack::<bool>();
    let new_value = !v_v1;
    s.put_stack(new_value);
}

fn format_bool(s: &mut State) {
    s.format_bool();
}

fn const_int(s: &mut State) {
    let v_val = *s.code::<i32>();
    let new_value = v_val;
    s.put_stack(new_value);
}

fn const_short(s: &mut State) {
    let v_val = *s.code::<i16>();
    let new_value = i32::from(v_val);
    s.put_stack(new_value);
}

fn const_tiny(s: &mut State) {
    let v_val = *s.code::<i8>();
    let new_value = i32::from(v_val);
    s.put_stack(new_value);
}

fn var_int(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<i32>(v_pos);
    s.put_stack(new_value);
}

fn put_int(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let v_value = *s.get_stack::<i32>();
    s.put_var(v_pos, v_value);
}

fn conv_int_from_null(s: &mut State) {
    let new_value = i32::MIN;
    s.put_stack(new_value);
}

fn const_long_text(s: &mut State) {
    let v_start = *s.code::<i32>();
    let v_size = *s.code::<i32>();
    s.string_from_texts(v_start, v_size);
}

fn cast_int_from_text(s: &mut State) {
    let v_v1 = s.string();
    let new_value = if let Ok(i) = v_v1.str().parse() {
        i
    } else {
        i32::MIN
    };
    s.put_stack(new_value);
}

fn cast_long_from_text(s: &mut State) {
    let v_v1 = s.string();
    let new_value = if let Ok(i) = v_v1.str().parse() {
        i
    } else {
        i64::MIN
    };
    s.put_stack(new_value);
}

fn cast_single_from_text(s: &mut State) {
    let v_v1 = s.string();
    let new_value = if let Ok(i) = v_v1.str().parse() {
        i
    } else {
        f32::NAN
    };
    s.put_stack(new_value);
}

fn cast_float_from_text(s: &mut State) {
    let v_v1 = s.string();
    let new_value = if let Ok(i) = v_v1.str().parse() {
        i
    } else {
        f64::NAN
    };
    s.put_stack(new_value);
}

fn abs_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_abs_int(v_v1);
    s.put_stack(new_value);
}

fn min_single_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_min_single_int(v_v1);
    s.put_stack(new_value);
}

fn conv_long_from_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_conv_long_from_int(v_v1);
    s.put_stack(new_value);
}

fn conv_float_from_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_conv_float_from_int(v_v1);
    s.put_stack(new_value);
}

fn conv_single_from_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_conv_single_from_int(v_v1);
    s.put_stack(new_value);
}

fn conv_bool_from_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_conv_bool_from_int(v_v1);
    s.put_stack(new_value);
}

fn add_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_add_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn min_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_min_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn mul_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_mul_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn div_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_div_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn rem_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_rem_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn land_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_logical_and_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn lor_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_logical_or_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn eor_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_exclusive_or_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn s_left_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_shift_left_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn s_right_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = external::op_shift_right_int(v_v1, v_v2);
    s.put_stack(new_value);
}

fn eq_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = v_v1 == v_v2;
    s.put_stack(new_value);
}

fn ne_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = v_v1 != v_v2;
    s.put_stack(new_value);
}

fn lt_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = v_v1 < v_v2;
    s.put_stack(new_value);
}

fn le_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = v_v1 <= v_v2;
    s.put_stack(new_value);
}

fn gt_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = v_v1 > v_v2;
    s.put_stack(new_value);
}

fn ge_int(s: &mut State) {
    let v_v2 = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<i32>();
    let new_value = v_v1 >= v_v2;
    s.put_stack(new_value);
}

fn format_int(s: &mut State) {
    s.format_int();
}

fn const_long(s: &mut State) {
    let v_val = *s.code::<i64>();
    let new_value = v_val;
    s.put_stack(new_value);
}

fn var_long(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<i64>(v_pos);
    s.put_stack(new_value);
}

fn put_long(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let v_value = *s.get_stack::<i64>();
    s.put_var(v_pos, v_value);
}

fn conv_long_from_null(s: &mut State) {
    let new_value = i64::MIN;
    s.put_stack(new_value);
}

fn abs_long(s: &mut State) {
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_abs_long(v_v1);
    s.put_stack(new_value);
}

fn min_single_long(s: &mut State) {
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_min_single_long(v_v1);
    s.put_stack(new_value);
}

fn cast_int_from_long(s: &mut State) {
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_cast_int_from_long(v_v1);
    s.put_stack(new_value);
}

fn conv_float_from_long(s: &mut State) {
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_conv_float_from_long(v_v1);
    s.put_stack(new_value);
}

fn conv_bool_from_long(s: &mut State) {
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_conv_bool_from_long(v_v1);
    s.put_stack(new_value);
}

fn add_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_add_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn min_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_min_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn mul_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_mul_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn div_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_div_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn rem_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_rem_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn land_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_logical_and_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn lor_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_logical_or_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn eor_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_exclusive_or_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn s_left_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_shift_left_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn s_right_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = external::op_shift_right_long(v_v1, v_v2);
    s.put_stack(new_value);
}

fn eq_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = v_v1 == v_v2;
    s.put_stack(new_value);
}

fn ne_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = v_v1 != v_v2;
    s.put_stack(new_value);
}

fn lt_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = v_v1 < v_v2;
    s.put_stack(new_value);
}

fn le_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = v_v1 <= v_v2;
    s.put_stack(new_value);
}

fn gt_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = v_v1 > v_v2;
    s.put_stack(new_value);
}

fn ge_long(s: &mut State) {
    let v_v2 = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<i64>();
    let new_value = v_v1 >= v_v2;
    s.put_stack(new_value);
}

fn format_long(s: &mut State) {
    s.format_long();
}

fn const_single(s: &mut State) {
    let v_val = *s.code::<f32>();
    let new_value = v_val;
    s.put_stack(new_value);
}

fn var_single(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<f32>(v_pos);
    s.put_stack(new_value);
}

fn put_single(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let v_value = *s.get_stack::<f32>();
    s.put_var(v_pos, v_value);
}

fn conv_single_from_null(s: &mut State) {
    let new_value = f32::NAN;
    s.put_stack(new_value);
}

fn abs_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.abs();
    s.put_stack(new_value);
}

fn min_single_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = -v_v1;
    s.put_stack(new_value);
}

fn cast_int_from_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = external::op_cast_int_from_single(v_v1);
    s.put_stack(new_value);
}

fn cast_long_from_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = external::op_cast_long_from_single(v_v1);
    s.put_stack(new_value);
}

fn conv_float_from_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = f64::from(v_v1);
    s.put_stack(new_value);
}

fn conv_bool_from_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = !v_v1.is_nan();
    s.put_stack(new_value);
}

fn add_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 + v_v2;
    s.put_stack(new_value);
}

fn min_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 - v_v2;
    s.put_stack(new_value);
}

fn mul_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 * v_v2;
    s.put_stack(new_value);
}

fn div_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 / v_v2;
    s.put_stack(new_value);
}

fn rem_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 % v_v2;
    s.put_stack(new_value);
}

fn math_cos_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.cos();
    s.put_stack(new_value);
}

fn math_sin_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.sin();
    s.put_stack(new_value);
}

fn math_tan_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.tan();
    s.put_stack(new_value);
}

fn math_acos_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.acos();
    s.put_stack(new_value);
}

fn math_asin_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.asin();
    s.put_stack(new_value);
}

fn math_atan_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.atan();
    s.put_stack(new_value);
}

fn math_atan2_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.atan2(v_v2);
    s.put_stack(new_value);
}

fn math_ceil_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.ceil();
    s.put_stack(new_value);
}

fn math_floor_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.floor();
    s.put_stack(new_value);
}

fn math_round_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.round();
    s.put_stack(new_value);
}

fn math_sqrt_single(s: &mut State) {
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.sqrt();
    s.put_stack(new_value);
}

fn math_log_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.log(v_v2);
    s.put_stack(new_value);
}

fn pow_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1.powf(v_v2);
    s.put_stack(new_value);
}

fn eq_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = (v_v1 - v_v2).abs() < 0.000_001f32;
    s.put_stack(new_value);
}

fn ne_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = (v_v1 - v_v2).abs() > 0.000_001f32;
    s.put_stack(new_value);
}

fn lt_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 < v_v2;
    s.put_stack(new_value);
}

fn le_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 <= v_v2;
    s.put_stack(new_value);
}

fn gt_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 > v_v2;
    s.put_stack(new_value);
}

fn ge_single(s: &mut State) {
    let v_v2 = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<f32>();
    let new_value = v_v1 >= v_v2;
    s.put_stack(new_value);
}

fn format_single(s: &mut State) {
    s.format_single();
}

fn const_float(s: &mut State) {
    let v_val = *s.code::<f64>();
    let new_value = v_val;
    s.put_stack(new_value);
}

fn var_float(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<f64>(v_pos);
    s.put_stack(new_value);
}

fn put_float(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let v_value = *s.get_stack::<f64>();
    s.put_var(v_pos, v_value);
}

fn conv_float_from_null(s: &mut State) {
    let new_value = f64::NAN;
    s.put_stack(new_value);
}

fn abs_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.abs();
    s.put_stack(new_value);
}

fn math_pi_float(s: &mut State) {
    let new_value = std::f64::consts::PI;
    s.put_stack(new_value);
}

fn math_e_float(s: &mut State) {
    let new_value = std::f64::consts::E;
    s.put_stack(new_value);
}

fn math_cos_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.cos();
    s.put_stack(new_value);
}

fn math_sin_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.sin();
    s.put_stack(new_value);
}

fn math_tan_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.tan();
    s.put_stack(new_value);
}

fn math_acos_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.acos();
    s.put_stack(new_value);
}

fn math_asin_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.asin();
    s.put_stack(new_value);
}

fn math_atan_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.atan();
    s.put_stack(new_value);
}

fn math_atan2_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.atan2(v_v2);
    s.put_stack(new_value);
}

fn math_ceil_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.ceil();
    s.put_stack(new_value);
}

fn math_floor_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.floor();
    s.put_stack(new_value);
}

fn math_round_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.round();
    s.put_stack(new_value);
}

fn math_sqrt_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.sqrt();
    s.put_stack(new_value);
}

fn math_log_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.log(v_v2);
    s.put_stack(new_value);
}

fn pow_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1.powf(v_v2);
    s.put_stack(new_value);
}

fn min_single_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = -v_v1;
    s.put_stack(new_value);
}

fn cast_single_from_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 as f32;
    s.put_stack(new_value);
}

fn cast_int_from_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = external::op_cast_int_from_float(v_v1);
    s.put_stack(new_value);
}

fn cast_long_from_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = external::op_cast_long_from_float(v_v1);
    s.put_stack(new_value);
}

fn conv_bool_from_float(s: &mut State) {
    let v_v1 = *s.get_stack::<f64>();
    let new_value = !v_v1.is_nan();
    s.put_stack(new_value);
}

fn add_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 + v_v2;
    s.put_stack(new_value);
}

fn min_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 - v_v2;
    s.put_stack(new_value);
}

fn mul_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 * v_v2;
    s.put_stack(new_value);
}

fn div_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 / v_v2;
    s.put_stack(new_value);
}

fn rem_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 % v_v2;
    s.put_stack(new_value);
}

fn eq_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = (v_v1 - v_v2).abs() < 0.000_000_001f64;
    s.put_stack(new_value);
}

fn ne_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = (v_v1 - v_v2).abs() > 0.000_000_001f64;
    s.put_stack(new_value);
}

fn lt_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 < v_v2;
    s.put_stack(new_value);
}

fn le_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 <= v_v2;
    s.put_stack(new_value);
}

fn gt_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 > v_v2;
    s.put_stack(new_value);
}

fn ge_float(s: &mut State) {
    let v_v2 = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<f64>();
    let new_value = v_v1 >= v_v2;
    s.put_stack(new_value);
}

fn format_float(s: &mut State) {
    s.format_float();
}

fn var_text(s: &mut State) {
    s.var_text();
}

fn arg_text(s: &mut State) {
    s.arg_text();
}

fn const_text(s: &mut State) {
    s.string_from_code();
}

fn conv_text_from_null(s: &mut State) {
    s.conv_text_from_null();
}

fn length_text(s: &mut State) {
    let v_v1 = s.string();
    let new_value = v_v1.str().len() as i32;
    s.put_stack(new_value);
}

fn conv_bool_from_text(s: &mut State) {
    let v_v1 = s.string();
    let new_value = !v_v1.str().is_empty();
    s.put_stack(new_value);
}

fn add_text(s: &mut State) {
    s.add_text();
}

fn text(s: &mut State) {
    s.text();
}

fn append_text(s: &mut State) {
    s.append_text();
}

fn get_text_sub(s: &mut State) {
    s.get_text_sub();
}

fn clear_text(s: &mut State) {
    s.clear_text();
}

fn free_text(s: &mut State) {
    s.free_text();
}

fn eq_text(s: &mut State) {
    let v_v2 = s.string();
    let v_v1 = s.string();
    let new_value = v_v1.str() == v_v2.str();
    s.put_stack(new_value);
}

fn ne_text(s: &mut State) {
    let v_v2 = s.string();
    let v_v1 = s.string();
    let new_value = v_v1.str() != v_v2.str();
    s.put_stack(new_value);
}

fn lt_text(s: &mut State) {
    let v_v2 = s.string();
    let v_v1 = s.string();
    let new_value = v_v1.str() < v_v2.str();
    s.put_stack(new_value);
}

fn le_text(s: &mut State) {
    let v_v2 = s.string();
    let v_v1 = s.string();
    let new_value = v_v1.str() <= v_v2.str();
    s.put_stack(new_value);
}

fn gt_text(s: &mut State) {
    let v_v2 = s.string();
    let v_v1 = s.string();
    let new_value = v_v1.str() > v_v2.str();
    s.put_stack(new_value);
}

fn ge_text(s: &mut State) {
    let v_v2 = s.string();
    let v_v1 = s.string();
    let new_value = v_v1.str() >= v_v2.str();
    s.put_stack(new_value);
}

fn format_text(s: &mut State) {
    s.format_text();
}

fn append_character(s: &mut State) {
    s.append_character();
}

fn var_enum(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<u8>(v_pos);
    s.put_stack(new_value);
}

fn const_enum(s: &mut State) {
    let v_val = *s.code::<u8>();
    let new_value = v_val;
    s.put_stack(new_value);
}

fn put_enum(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let v_value = *s.get_stack::<u8>();
    s.put_var(v_pos, v_value);
}

fn conv_bool_from_enum(s: &mut State) {
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 != 255;
    s.put_stack(new_value);
}

fn cast_text_from_enum(s: &mut State) {
    let v_enum_tp = *s.get_stack::<u16>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = Str::new(&s.database.enum_val(v_enum_tp, v_v1));
    s.put_stack(new_value);
}

fn cast_enum_from_text(s: &mut State) {
    let v_enum_tp = *s.code::<u16>();
    let v_v1 = s.string();
    let new_value = s.database.to_enum(v_enum_tp, v_v1.str());
    s.put_stack(new_value);
}

fn cast_int_from_enum(s: &mut State) {
    let v_v1 = *s.get_stack::<u8>();
    let new_value = if v_v1 == 255 {
        i32::MIN
    } else {
        i32::from(v_v1)
    };
    s.put_stack(new_value);
}

fn cast_enum_from_int(s: &mut State) {
    let v_v1 = *s.get_stack::<i32>();
    let new_value = if v_v1 == i32::MIN { 255 } else { v_v1 as u8 };
    s.put_stack(new_value);
}

fn conv_enum_from_null(s: &mut State) {
    let new_value = 255u8;
    s.put_stack(new_value);
}

fn eq_enum(s: &mut State) {
    let v_v2 = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 == v_v2;
    s.put_stack(new_value);
}

fn ne_enum(s: &mut State) {
    let v_v2 = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 != v_v2;
    s.put_stack(new_value);
}

fn lt_enum(s: &mut State) {
    let v_v2 = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 < v_v2;
    s.put_stack(new_value);
}

fn le_enum(s: &mut State) {
    let v_v2 = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 <= v_v2;
    s.put_stack(new_value);
}

fn gt_enum(s: &mut State) {
    let v_v2 = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 > v_v2;
    s.put_stack(new_value);
}

fn ge_enum(s: &mut State) {
    let v_v2 = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<u8>();
    let new_value = v_v1 >= v_v2;
    s.put_stack(new_value);
}

fn database(s: &mut State) {
    s.database();
}

fn format_database(s: &mut State) {
    s.format_database();
}

fn conv_bool_from_ref(s: &mut State) {
    let v_val = *s.get_stack::<DbRef>();
    let new_value = v_val.rec != 0;
    s.put_stack(new_value);
}

fn conv_ref_from_null(s: &mut State) {
    let new_value = s.database.null();
    s.put_stack(new_value);
}

fn free_ref(s: &mut State) {
    s.free_ref();
}

fn append(s: &mut State) {
    s.append();
}

fn var_ref(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = {
        let r = *s.get_var::<DbRef>(v_pos);
        s.database.valid(&r);
        r
    };
    s.put_stack(new_value);
}

fn put_ref(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let v_value = *s.get_stack::<DbRef>();
    s.put_var(v_pos, v_value);
}

fn eq_ref(s: &mut State) {
    let v_v2 = *s.get_stack::<DbRef>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = v_v1 == v_v2;
    s.put_stack(new_value);
}

fn ne_ref(s: &mut State) {
    let v_v2 = *s.get_stack::<DbRef>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = v_v1 != v_v2;
    s.put_stack(new_value);
}

fn get_ref(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = s.database.get_ref(&v_v1, u32::from(v_fld));
    s.put_stack(new_value);
}

fn set_ref(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = *s.get_stack::<DbRef>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database
            .store_mut(&db)
            .set_int(db.rec, db.pos + u32::from(v_fld), v_val.rec as i32);
    }
}

fn get_field(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = DbRef {
        store_nr: v_v1.store_nr,
        rec: v_v1.rec,
        pos: v_v1.pos + u32::from(v_fld),
    };
    s.put_stack(new_value);
}

fn get_int(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        if db.rec == 0 {
            i32::MIN
        } else {
            s.database
                .store(&db)
                .get_int(db.rec, db.pos + u32::from(v_fld))
        }
    };
    s.put_stack(new_value);
}

fn get_long(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        s.database
            .store(&db)
            .get_long(db.rec, db.pos + u32::from(v_fld))
    };
    s.put_stack(new_value);
}

fn get_single(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        s.database
            .store(&db)
            .get_single(db.rec, db.pos + u32::from(v_fld))
    };
    s.put_stack(new_value);
}

fn get_float(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        s.database
            .store(&db)
            .get_float(db.rec, db.pos + u32::from(v_fld))
    };
    s.put_stack(new_value);
}

fn get_byte(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_min = *s.code::<i16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        s.database
            .store(&db)
            .get_byte(db.rec, db.pos + u32::from(v_fld), i32::from(v_min))
    };
    s.put_stack(new_value);
}

fn get_enum(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        s.database
            .store(&db)
            .get_byte(db.rec, db.pos + u32::from(v_fld), 0) as u8
    };
    s.put_stack(new_value);
}

fn set_enum(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = *s.get_stack::<u8>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database
            .store_mut(&db)
            .set_byte(db.rec, db.pos + u32::from(v_fld), 0, i32::from(v_val));
    }
}

fn get_short(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_min = *s.code::<i16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        s.database
            .store(&db)
            .get_short(db.rec, db.pos + u32::from(v_fld), i32::from(v_min))
    };
    s.put_stack(new_value);
}

fn get_text(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_v1 = *s.get_stack::<DbRef>();
    let new_value = {
        let db = v_v1;
        let store = s.database.store(&db);
        Str::new(store.get_str(store.get_int(db.rec, db.pos + u32::from(v_fld)) as u32))
    };
    s.put_stack(new_value);
}

fn set_int(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database
            .store_mut(&db)
            .set_int(db.rec, db.pos + u32::from(v_fld), v_val);
    }
}

fn set_long(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = *s.get_stack::<i64>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database
            .store_mut(&db)
            .set_long(db.rec, db.pos + u32::from(v_fld), v_val);
    }
}

fn set_single(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = *s.get_stack::<f32>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database
            .store_mut(&db)
            .set_single(db.rec, db.pos + u32::from(v_fld), v_val);
    }
}

fn set_float(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = *s.get_stack::<f64>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database
            .store_mut(&db)
            .set_float(db.rec, db.pos + u32::from(v_fld), v_val);
    }
}

fn set_byte(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_min = *s.code::<i16>();
    let v_val = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database.store_mut(&db).set_byte(
            db.rec,
            db.pos + u32::from(v_fld),
            i32::from(v_min),
            v_val,
        );
    }
}

fn set_short(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_min = *s.code::<i16>();
    let v_val = *s.get_stack::<i32>();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        s.database.store_mut(&db).set_short(
            db.rec,
            db.pos + u32::from(v_fld),
            i32::from(v_min),
            v_val,
        );
    }
}

fn set_text(s: &mut State) {
    let v_fld = *s.code::<u16>();
    let v_val = s.string();
    let v_v1 = *s.get_stack::<DbRef>();
    {
        let db = v_v1;
        let s_val = v_val.str().to_string();
        let store = s.database.store_mut(&db);
        let s_pos = store.set_str(&s_val);
        store.set_int(db.rec, db.pos + u32::from(v_fld), s_pos as i32);
    }
}

fn var_vector(s: &mut State) {
    let v_pos = *s.code::<u16>();
    let new_value = *s.get_var::<DbRef>(v_pos);
    s.put_stack(new_value);
}

fn length_vector(s: &mut State) {
    let v_r = *s.get_stack::<DbRef>();
    let new_value = vector::length_vector(&v_r, &s.database.allocations) as i32;
    s.put_stack(new_value);
}

fn clear_vector(s: &mut State) {
    let v_r = *s.get_stack::<DbRef>();
    vector::clear_vector(&v_r, &mut s.database.allocations);
}

fn get_vector(s: &mut State) {
    let v_size = *s.code::<u16>();
    let v_index = *s.get_stack::<i32>();
    let v_r = *s.get_stack::<DbRef>();
    let new_value = vector::get_vector(&v_r, u32::from(v_size), v_index, &s.database.allocations);
    s.put_stack(new_value);
}

fn cast_vector_from_text(s: &mut State) {
    s.cast_vector_from_text();
}

fn remove_vector(s: &mut State) {
    let v_size = *s.code::<u16>();
    let v_index = *s.get_stack::<i32>();
    let v_r = *s.get_stack::<DbRef>();
    let new_value = vector::remove_vector(
        &v_r,
        u32::from(v_size),
        v_index as u32,
        &mut s.database.allocations,
    );
    s.put_stack(new_value);
}

fn insert_vector(s: &mut State) {
    s.insert_vector();
}

fn new_record(s: &mut State) {
    s.new_record();
}

fn finish_record(s: &mut State) {
    s.finish_record();
}

fn append_vector(s: &mut State) {
    let v_tp = *s.code::<u16>();
    let v_other = *s.get_stack::<DbRef>();
    let v_r = *s.get_stack::<DbRef>();
    s.database.vector_add(&v_r, &v_other, v_tp);
}

fn get_record(s: &mut State) {
    s.get_record();
}

fn validate(s: &mut State) {
    s.validate();
}

fn hash_add(s: &mut State) {
    s.hash_add();
}

fn hash_find(s: &mut State) {
    s.hash_find();
}

fn hash_remove(s: &mut State) {
    s.hash_remove();
}

fn eq_bool(s: &mut State) {
    let v_v2 = *s.get_stack::<bool>();
    let v_v1 = *s.get_stack::<bool>();
    let new_value = v_v1 == v_v2;
    s.put_stack(new_value);
}

fn ne_bool(s: &mut State) {
    let v_v2 = *s.get_stack::<bool>();
    let v_v1 = *s.get_stack::<bool>();
    let new_value = v_v1 != v_v2;
    s.put_stack(new_value);
}

fn panic(s: &mut State) {
    let v_message = s.string();
    panic!("{}", v_message.str());
}

fn print(s: &mut State) {
    let v_v1 = s.string();
    print!("{}", v_v1.str());
}

fn iterate(s: &mut State) {
    s.iterate();
}

fn step(s: &mut State) {
    s.step();
}

fn remove(s: &mut State) {
    s.remove();
}

fn append_copy(s: &mut State) {
    s.append_copy();
}

fn copy_record(s: &mut State) {
    s.copy_record();
}

fn static_call(s: &mut State) {
    s.static_call();
}

fn create_ref(s: &mut State) {
    s.create_ref();
}

fn get_ref_text(s: &mut State) {
    s.get_ref_text();
}

fn get_db_ref(s: &mut State) {
    s.get_db_ref();
}

fn set_db_ref(s: &mut State) {
    s.set_db_ref();
}

fn append_ref_text(s: &mut State) {
    s.append_ref_text();
}

fn clear_ref_text(s: &mut State) {
    s.clear_ref_text();
}

fn get_file(s: &mut State) {
    let v_file = *s.get_stack::<DbRef>();
    let new_value = s.database.get_file(&v_file);
    s.put_stack(new_value);
}

fn get_dir(s: &mut State) {
    let v_result = *s.get_stack::<DbRef>();
    let v_path = s.string();
    let new_value = s.database.get_dir(v_path.str(), &v_result);
    s.put_stack(new_value);
}

fn get_png_image(s: &mut State) {
    let v_image = *s.get_stack::<DbRef>();
    let v_path = s.string();
    let new_value = s.database.get_png(v_path.str(), &v_image);
    s.put_stack(new_value);
}

fn get_file_text(s: &mut State) {
    s.get_file_text();
}
