// Copyright (c) 2024-2026 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Tests for file system functions and binary file #read / #write operations.
extern crate dryopea;

mod testing;

use dryopea::data::Value;

#[test]
fn files() {
    expr!("\"{file(\"example\").files()}\"").result(Value::str(
        "[\
{path:\"example/config\",format:Directory},\
{path:\"example/map.png\",size:3406,format:TextFile},\
{path:\"example/map.xcf\",size:7817,format:TextFile},\
{path:\"example/show.loft\",size:371,format:TextFile},\
{path:\"example/todo.json\",size:1461,format:TextFile}]",
    ));
}

// --- file() format detection ---

#[test]
fn file_text_format() {
    expr!("\"{file(\"example/show.loft\").format}\"").result(Value::str("TextFile"));
}

#[test]
fn file_dir_format() {
    expr!("\"{file(\"example\").format}\"").result(Value::str("Directory"));
}

#[test]
fn file_not_found() {
    expr!("\"{file(\"nonexistent_file_xyz.txt\").format}\"").result(Value::str("NotExists"));
}

// --- path validation in file() ---

#[test]
fn file_outside_project() {
    // Direct ".." at the start must be rejected.
    expr!("\"{file(\"../Cargo.toml\").format}\"").result(Value::str("NotExists"));
}

#[test]
fn file_dotdot_net_upward() {
    // Two ".." segments exceed available depth → rejected.
    expr!("\"{file(\"example/../../etc\").format}\"").result(Value::str("NotExists"));
}

#[test]
fn file_dotdot_within_project() {
    // "example/../example/show.loft" stays within the project (net depth >= 0).
    expr!("\"{file(\"example/../example/show.loft\").format}\"").result(Value::str("TextFile"));
}

// --- exists ---

#[test]
fn exists_true_file() {
    expr!("if exists(\"example/show.loft\") { 1 } else { 0 }").result(Value::Int(1));
}

#[test]
fn exists_true_dir() {
    expr!("if exists(\"example\") { 1 } else { 0 }").result(Value::Int(1));
}

#[test]
fn exists_missing() {
    expr!("if !exists(\"nonexistent_file_xyz.txt\") { 1 } else { 0 }").result(Value::Int(1));
}

#[test]
fn exists_outside_project() {
    expr!("if !exists(\"../Cargo.toml\") { 1 } else { 0 }").result(Value::Int(1));
}

// --- delete ---

#[test]
fn delete_nonexistent() {
    expr!("if !delete(\"nonexistent_file_xyz.txt\") { 1 } else { 0 }").result(Value::Int(1));
}

#[test]
fn delete_outside_project() {
    expr!("if !delete(\"../Cargo.toml\") { 1 } else { 0 }").result(Value::Int(1));
}

// --- move ---

#[test]
fn move_missing_source() {
    expr!("if !move(\"nonexistent_src_xyz.txt\", \"somewhere_xyz.txt\") { 1 } else { 0 }")
        .result(Value::Int(1));
}

#[test]
fn move_dest_already_exists() {
    // Destination already exists → must refuse.
    expr!("if !move(\"example/show.loft\", \"example/map.png\") { 1 } else { 0 }")
        .result(Value::Int(1));
}

#[test]
fn move_dest_outside_project() {
    // Destination outside the project → must refuse even though the source is valid.
    expr!("if !move(\"example/show.loft\", \"../outside_target.txt\") { 1 } else { 0 }")
        .result(Value::Int(1));
}

#[test]
fn move_source_outside_project() {
    // Source outside the project → must refuse.
    expr!("if !move(\"../Cargo.toml\", \"stolen.toml\") { 1 } else { 0 }").result(Value::Int(1));
}

// --- write + delete cycle ---

#[test]
fn write_and_delete() {
    expr!(
        "delete(\"test_fs_wd.txt\");
f = file(\"test_fs_wd.txt\");
f.write(\"hello\");
was = exists(\"test_fs_wd.txt\");
delete(\"test_fs_wd.txt\");
if was && !exists(\"test_fs_wd.txt\") { 1 } else { 0 }"
    )
    .result(Value::Int(1));
}

// --- write + move cycle ---

#[test]
fn write_and_move() {
    expr!(
        "delete(\"test_fs_wm_dst.txt\");
f = file(\"test_fs_wm_src.txt\");
f.write(\"data\");
ok = move(\"test_fs_wm_src.txt\", \"test_fs_wm_dst.txt\");
delete(\"test_fs_wm_dst.txt\");
if ok { 1 } else { 0 }"
    )
    .result(Value::Int(1));
}

#[test]
fn read_file() {
    expr!(
        "f = file(\"example/todo.json\");
f.format = Format.LittleEndian;
r = f#read(1) as u8;
r
"
    )
    .result(Value::Int(123));
}

// --- binary #write (f += value) ---

/// Writing an integer to a binary file creates the file.
#[test]
fn write_int_creates_file() {
    expr!(
        "delete(\"test_bin_wi.bin\");
f = file(\"test_bin_wi.bin\");
f.format = Format.LittleEndian;
f += 42;
result = exists(\"test_bin_wi.bin\");
delete(\"test_bin_wi.bin\");
result"
    )
    .result(Value::Int(1))
    .tp(dryopea::data::Type::Boolean);
}

/// Writing an integer in LittleEndian and reading it back gives the original value.
#[test]
fn write_read_int_little_endian() {
    expr!(
        "delete(\"test_bin_wrile.bin\");
f = file(\"test_bin_wrile.bin\");
f.format = Format.LittleEndian;
f += 0x11223344;
g = file(\"test_bin_wrile.bin\");
g.format = Format.LittleEndian;
result = g#read(4) as i32;
delete(\"test_bin_wrile.bin\");
result"
    )
    .result(Value::Int(0x11223344));
}

/// Writing an integer in BigEndian and reading it back gives the original value.
#[test]
fn write_read_int_big_endian() {
    expr!(
        "delete(\"test_bin_wribe.bin\");
f = file(\"test_bin_wribe.bin\");
f.format = Format.BigEndian;
f += 0x11223344;
g = file(\"test_bin_wribe.bin\");
g.format = Format.BigEndian;
result = g#read(4) as i32;
delete(\"test_bin_wribe.bin\");
result"
    )
    .result(Value::Int(0x11223344));
}

/// Writing in BigEndian and reading in LittleEndian gives a byte-swapped result.
#[test]
fn big_endian_vs_little_endian_byte_order() {
    expr!(
        "delete(\"test_bin_be.bin\");
f = file(\"test_bin_be.bin\");
f.format = Format.BigEndian;
f += 0x01000000;
g = file(\"test_bin_be.bin\");
g.format = Format.LittleEndian;
result = g#read(4) as i32;
delete(\"test_bin_be.bin\");
result"
    )
    .result(Value::Int(1));
}

/// Writing a long (8 bytes) and reading it back gives the original value.
#[test]
fn write_read_long() {
    expr!(
        "delete(\"test_bin_wrl.bin\");
f = file(\"test_bin_wrl.bin\");
f.format = Format.LittleEndian;
f += 0x0102030405060708l;
g = file(\"test_bin_wrl.bin\");
g.format = Format.LittleEndian;
result = g#read(8) as long;
delete(\"test_bin_wrl.bin\");
result"
    )
    .result(Value::Long(0x0102030405060708));
}

// Note: binary text write (f += "Hello") is not yet supported because text
// is stored as a Str fat pointer on the stack, not as a record reference that
// read_data can dereference. Use f.write(text) for text-mode file writes instead.

// --- f#size ---

/// f#size returns the number of bytes written to the file.
#[test]
fn size_after_one_int() {
    expr!(
        "delete(\"test_bin_sz1.bin\");
f = file(\"test_bin_sz1.bin\");
f.format = Format.LittleEndian;
f += 0;
result = f#size;
delete(\"test_bin_sz1.bin\");
result"
    )
    .result(Value::Long(4));
}

/// f#size reflects two consecutive integer writes (8 bytes total).
#[test]
fn size_after_two_ints() {
    expr!(
        "delete(\"test_bin_sz2.bin\");
f = file(\"test_bin_sz2.bin\");
f.format = Format.LittleEndian;
f += 1;
f += 2;
result = f#size;
delete(\"test_bin_sz2.bin\");
result"
    )
    .result(Value::Long(8));
}

/// file().size field (filled by file()) reflects the written size.
#[test]
fn size_via_read_handle() {
    expr!(
        "delete(\"test_bin_szr.bin\");
f = file(\"test_bin_szr.bin\");
f.format = Format.LittleEndian;
f += 42;
result = file(\"test_bin_szr.bin\").size;
delete(\"test_bin_szr.bin\");
result"
    )
    .result(Value::Long(4));
}

// --- f#index and f#next ---

/// After reading 4 bytes, f#index (start of last read) is 0.
#[test]
fn index_after_first_read() {
    expr!(
        "delete(\"test_bin_idx.bin\");
f = file(\"test_bin_idx.bin\");
f.format = Format.LittleEndian;
f += 42;
g = file(\"test_bin_idx.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
result = g#index;
delete(\"test_bin_idx.bin\");
result"
    )
    .result(Value::Long(0));
}

/// After reading 4 bytes, f#next (current position) is 4.
#[test]
fn next_after_first_read() {
    expr!(
        "delete(\"test_bin_nxt.bin\");
f = file(\"test_bin_nxt.bin\");
f.format = Format.LittleEndian;
f += 99;
g = file(\"test_bin_nxt.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
result = g#next;
delete(\"test_bin_nxt.bin\");
result"
    )
    .result(Value::Long(4));
}

/// After two reads, f#index reflects the start of the second read.
#[test]
fn index_after_second_read() {
    expr!(
        "delete(\"test_bin_idx2.bin\");
f = file(\"test_bin_idx2.bin\");
f.format = Format.LittleEndian;
f += 1;
f += 2;
g = file(\"test_bin_idx2.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
g#read(4) as i32;
result = g#index;
delete(\"test_bin_idx2.bin\");
result"
    )
    .result(Value::Long(4));
}

/// After two reads, f#next is at 8.
#[test]
fn next_after_second_read() {
    expr!(
        "delete(\"test_bin_nxt2.bin\");
f = file(\"test_bin_nxt2.bin\");
f.format = Format.LittleEndian;
f += 1;
f += 2;
g = file(\"test_bin_nxt2.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
g#read(4) as i32;
result = g#next;
delete(\"test_bin_nxt2.bin\");
result"
    )
    .result(Value::Long(8));
}

// --- f#next = pos (seek) ---

/// Setting f#next = 0 seeks back to the start; subsequent read returns the first value again.
#[test]
fn seek_to_start_rereads_first_value() {
    expr!(
        "delete(\"test_bin_seek.bin\");
f = file(\"test_bin_seek.bin\");
f.format = Format.LittleEndian;
f += 0x11223344;
f += 0x55667788;
g = file(\"test_bin_seek.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
g#read(4) as i32;
g#next = 0;
result = g#read(4) as i32;
delete(\"test_bin_seek.bin\");
result"
    )
    .result(Value::Int(0x11223344));
}

/// Seeking to offset 4 (after opening the file with reads) reads the second integer.
/// Note: seek only works when the file is already open; seeking before the first read
/// has no effect because the underlying file handle does not exist yet.
#[test]
fn seek_to_offset_reads_second_value() {
    expr!(
        "delete(\"test_bin_seek4.bin\");
f = file(\"test_bin_seek4.bin\");
f.format = Format.LittleEndian;
f += 0x11223344;
f += 0x55667788;
g = file(\"test_bin_seek4.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
g#read(4) as i32;
g#next = 4;
result = g#read(4) as i32;
delete(\"test_bin_seek4.bin\");
result"
    )
    .result(Value::Int(0x55667788));
}

// --- text binary write and read ---

/// Writing a text value in binary mode writes the raw UTF-8 bytes (no length prefix).
/// Reading them back with f#read(n) as text returns the same string.
#[test]
fn write_read_text_bytes() {
    expr!(
        "delete(\"test_bin_txt.bin\");
f = file(\"test_bin_txt.bin\");
f.format = Format.LittleEndian;
f += \"Hello\";
g = file(\"test_bin_txt.bin\");
g.format = Format.LittleEndian;
result = g#read(5) as text;
delete(\"test_bin_txt.bin\");
result"
    )
    .result(Value::str("Hello"));
}

/// Reading past EOF with a text read returns the partial content.
#[test]
fn read_past_eof_text_returns_partial() {
    expr!(
        "delete(\"test_bin_ptxt.bin\");
f = file(\"test_bin_ptxt.bin\");
f.format = Format.LittleEndian;
f += \"Hi\";
g = file(\"test_bin_ptxt.bin\");
g.format = Format.LittleEndian;
result = g#read(100) as text;
delete(\"test_bin_ptxt.bin\");
result"
    )
    .result(Value::str("Hi"));
}

// --- partial / overflow reads ---

/// Reading too few bytes for a typed non-text value fails and leaves f#next unchanged.
/// Write 4 bytes (one integer), then request 8 bytes as long: read_exact fails → next stays 0.
#[test]
fn incomplete_typed_read_is_null() {
    expr!(
        "delete(\"test_bin_null.bin\");
f = file(\"test_bin_null.bin\");
f.format = Format.LittleEndian;
f += 42;
g = file(\"test_bin_null.bin\");
g.format = Format.LittleEndian;
g#read(8) as long;
result = if g#next == 0 { 1 } else { 0 };
delete(\"test_bin_null.bin\");
result"
    )
    .result(Value::Int(1));
}

#[test]
fn array_write() {
    expr!(
        "buf = file(\"buffer2.bin\");
buf#format = LittleEndian;
buf += [ 1, 2 ];
delete(\"buffer2.bin\");
2
"
    )
    .result(Value::Int(2));
}

/// BigEndian sequential write: u8(0), u8(1), u16(0x0203), i32(0x04050607) produce
/// bytes 0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07; byte at offset 4 is 0x04.
#[test]
fn bigendian_sequential_bytes() {
    expr!(
        "delete(\"test_bin_be_seq.bin\");
f = file(\"test_bin_be_seq.bin\");
f.format = Format.BigEndian;
f += 0 as u8;
f += 1 as u8;
f += 0x203 as u16;
f += 0x4050607;
g = file(\"test_bin_be_seq.bin\");
g.format = Format.LittleEndian;
g#read(4) as i32;
result = g#read(1) as u8;
delete(\"test_bin_be_seq.bin\");
result"
    )
    .result(Value::Int(4));
}
