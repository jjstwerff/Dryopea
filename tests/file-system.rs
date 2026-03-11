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

// Note: f += "text" writes raw UTF-8 bytes. Binary modes (LittleEndian/BigEndian) and
// TextFile mode both support text writes. TextFile is the default for new files.

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

// --- text file partial read and write via #read / += ---

/// Writing text to a TextFile with += appends the raw UTF-8 bytes.
/// Reading them back with #read(n) as text returns the same string.
#[test]
fn text_file_write_read() {
    expr!(
        "delete(\"test_tf_wr.txt\");
f = file(\"test_tf_wr.txt\");
f +=\"Hello\";
g = file(\"test_tf_wr.txt\");
result = g#read(5) as text;
delete(\"test_tf_wr.txt\");
result"
    )
    .result(Value::str("Hello"));
}

/// Multiple += calls on a TextFile append sequentially.
#[test]
fn text_file_append_multiple() {
    expr!(
        "delete(\"test_tf_am.txt\");
f = file(\"test_tf_am.txt\");
f +=\"Hel\";
f += \"lo\";
g = file(\"test_tf_am.txt\");
result = g#read(5) as text;
delete(\"test_tf_am.txt\");
result"
    )
    .result(Value::str("Hello"));
}

/// f#read(n) on a TextFile with n larger than the file returns the partial content.
#[test]
fn text_file_read_partial_eof() {
    expr!(
        "delete(\"test_tf_pe.txt\");
f = file(\"test_tf_pe.txt\");
f +=\"Hi\";
g = file(\"test_tf_pe.txt\");
result = g#read(100) as text;
delete(\"test_tf_pe.txt\");
result"
    )
    .result(Value::str("Hi"));
}

/// After reading from a TextFile, g#index holds the byte offset where the read started.
#[test]
fn text_file_index_after_read() {
    expr!(
        "delete(\"test_tf_idx.txt\");
f = file(\"test_tf_idx.txt\");
f +=\"Hello\";
g = file(\"test_tf_idx.txt\");
g#read(3) as text;
result = g#index;
delete(\"test_tf_idx.txt\");
result"
    )
    .result(Value::Long(0));
}

/// After reading from a TextFile, g#next holds the byte offset after the read.
#[test]
fn text_file_next_after_read() {
    expr!(
        "delete(\"test_tf_nxt.txt\");
f = file(\"test_tf_nxt.txt\");
f +=\"Hello\";
g = file(\"test_tf_nxt.txt\");
g#read(3) as text;
result = g#next;
delete(\"test_tf_nxt.txt\");
result"
    )
    .result(Value::Long(3));
}

/// After two reads, g#index points to the start of the second read and g#next to after it.
#[test]
fn text_file_index_next_after_two_reads() {
    expr!(
        "delete(\"test_tf_2r.txt\");
f = file(\"test_tf_2r.txt\");
f +=\"Hello World\";
g = file(\"test_tf_2r.txt\");
g#read(5) as text;
g#read(6) as text;
idx = g#index;
nxt = g#next;
delete(\"test_tf_2r.txt\");
\"{idx} {nxt}\""
    )
    .result(Value::str("5 11"));
}

/// Setting g#next = 0 on a TextFile seeks back to the start; the next read returns
/// from the beginning again.
#[test]
fn text_file_seek_rerereads_start() {
    expr!(
        "delete(\"test_tf_sk.txt\");
f = file(\"test_tf_sk.txt\");
f +=\"Hello\";
g = file(\"test_tf_sk.txt\");
g#read(5) as text;
g#next = 0l;
result = g#read(5) as text;
delete(\"test_tf_sk.txt\");
result"
    )
    .result(Value::str("Hello"));
}

/// Seeking into the middle of a TextFile reads from that offset.
#[test]
fn text_file_seek_to_offset() {
    expr!(
        "delete(\"test_tf_so.txt\");
f = file(\"test_tf_so.txt\");
f +=\"Hello World\";
g = file(\"test_tf_so.txt\");
g#read(5) as text;
g#next = 6l;
result = g#read(5) as text;
delete(\"test_tf_so.txt\");
result"
    )
    .result(Value::str("World"));
}

// --- #index and #next tracking on writes (binary and text) ---
// Writes update the same fields as reads:
//   #index = byte offset where the write started
//   #next  = byte offset immediately after the write

/// After one binary integer write, f#index == 0 (write started at start of file).
#[test]
fn write_binary_index_after_first() {
    expr!(
        "delete(\"test_wbi1.bin\");
f = file(\"test_wbi1.bin\");
f.format = Format.LittleEndian;
f += 42;
result = f#index;
delete(\"test_wbi1.bin\");
result"
    )
    .result(Value::Long(0));
}

/// After one binary integer write, f#next == 4 (4 bytes written).
#[test]
fn write_binary_next_after_first() {
    expr!(
        "delete(\"test_wbn1.bin\");
f = file(\"test_wbn1.bin\");
f.format = Format.LittleEndian;
f += 42;
result = f#next;
delete(\"test_wbn1.bin\");
result"
    )
    .result(Value::Long(4));
}

/// After two binary integer writes, f#index == 4 (second write started at byte 4).
#[test]
fn write_binary_index_after_second() {
    expr!(
        "delete(\"test_wbi2.bin\");
f = file(\"test_wbi2.bin\");
f.format = Format.LittleEndian;
f += 1;
f += 2;
result = f#index;
delete(\"test_wbi2.bin\");
result"
    )
    .result(Value::Long(4));
}

/// After two binary integer writes, f#next == 8 (8 bytes written total).
#[test]
fn write_binary_next_after_second() {
    expr!(
        "delete(\"test_wbn2.bin\");
f = file(\"test_wbn2.bin\");
f.format = Format.LittleEndian;
f += 1;
f += 2;
result = f#next;
delete(\"test_wbn2.bin\");
result"
    )
    .result(Value::Long(8));
}

/// After writing "Hello" to a TextFile, f#index == 0 and f#next == 5.
#[test]
fn write_text_index_next() {
    expr!(
        "delete(\"test_wti.txt\");
f = file(\"test_wti.txt\");
f += \"Hello\";
idx = f#index;
nxt = f#next;
delete(\"test_wti.txt\");
\"{idx} {nxt}\""
    )
    .result(Value::str("0 5"));
}

/// After writing \"Hel\" then \"lo\", f#index == 3 (second write started at 3), f#next == 5.
#[test]
fn write_text_index_next_two_writes() {
    expr!(
        "delete(\"test_wt2.txt\");
f = file(\"test_wt2.txt\");
f += \"Hel\";
f += \"lo\";
idx = f#index;
nxt = f#next;
delete(\"test_wt2.txt\");
\"{idx} {nxt}\""
    )
    .result(Value::str("3 5"));
}

/// Seeking the write handle back to 0 and writing again overwrites from the start.
/// This works identically for binary and text files (both opened with File::create).
#[test]
fn write_seek_overwrites_binary() {
    expr!(
        "delete(\"test_wso.bin\");
f = file(\"test_wso.bin\");
f.format = Format.LittleEndian;
f += 0x11223344;
f#next = 0l;
f += 0x55667788;
g = file(\"test_wso.bin\");
g.format = Format.LittleEndian;
result = g#read(4) as i32;
delete(\"test_wso.bin\");
result"
    )
    .result(Value::Int(0x55667788));
}

/// Same seek-and-overwrite behaviour for a TextFile write handle.
#[test]
fn write_seek_overwrites_text() {
    expr!(
        "delete(\"test_wsot.txt\");
f = file(\"test_wsot.txt\");
f += \"Hello\";
f#next = 0l;
f += \"World\";
g = file(\"test_wsot.txt\");
result = g#read(5) as text;
delete(\"test_wsot.txt\");
result"
    )
    .result(Value::str("World"));
}

// --- f#size = n: truncate and extend ---

/// Truncating a text file to fewer bytes discards the tail.
#[test]
fn set_size_truncates_text() {
    expr!(
        "delete(\"test_sz_tr.txt\");
f = file(\"test_sz_tr.txt\");
f += \"Hello World\";
f#size = 5l;
g = file(\"test_sz_tr.txt\");
result = g#read(100) as text;
delete(\"test_sz_tr.txt\");
result"
    )
    .result(Value::str("Hello"));
}

/// Truncating a binary file: write two ints (8 bytes), resize to 4 — only the first remains.
#[test]
fn set_size_truncates_binary() {
    expr!(
        "delete(\"test_sz_tb.bin\");
f = file(\"test_sz_tb.bin\");
f.format = Format.LittleEndian;
f += 0x11223344;
f += 0x55667788;
f#size = 4l;
g = file(\"test_sz_tb.bin\");
g.format = Format.LittleEndian;
result = g#read(4) as i32;
delete(\"test_sz_tb.bin\");
result"
    )
    .result(Value::Int(0x11223344));
}

/// After truncation the file size reported by f#size matches the new length.
#[test]
fn set_size_reports_new_size() {
    expr!(
        "delete(\"test_sz_rs.txt\");
f = file(\"test_sz_rs.txt\");
f += \"Hello World\";
f#size = 5l;
g = file(\"test_sz_rs.txt\");
result = g#size;
delete(\"test_sz_rs.txt\");
result"
    )
    .result(Value::Long(5));
}

/// Extending a file pads with null bytes; the reported size grows to the requested length.
#[test]
fn set_size_extends_file() {
    expr!(
        "delete(\"test_sz_ex.txt\");
f = file(\"test_sz_ex.txt\");
f += \"Hi\";
f#size = 10l;
g = file(\"test_sz_ex.txt\");
result = g#size;
delete(\"test_sz_ex.txt\");
result"
    )
    .result(Value::Long(10));
}

/// Setting f#size = 0 empties the file completely.
#[test]
fn set_size_zero_clears_file() {
    expr!(
        "delete(\"test_sz_z.txt\");
f = file(\"test_sz_z.txt\");
f += \"Hello\";
f#size = 0l;
g = file(\"test_sz_z.txt\");
result = g#size;
delete(\"test_sz_z.txt\");
result"
    )
    .result(Value::Long(0));
}

/// Negative size is rejected: set_file_size returns false and leaves the file unchanged.
#[test]
fn set_size_negative_fails() {
    expr!(
        "delete(\"test_sz_neg.txt\");
f = file(\"test_sz_neg.txt\");
f += \"Hello\";
ok = f.set_file_size(-1l);
g = file(\"test_sz_neg.txt\");
result = g#size;
delete(\"test_sz_neg.txt\");
if !ok && result == 5l { 1 } else { 0 }"
    )
    .result(Value::Int(1));
}

/// Applying f#size to a directory handle returns false.
#[test]
fn set_size_directory_fails() {
    expr!(
        "d = file(\"example\");
ok = d.set_file_size(0l);
if !ok { 1 } else { 0 }"
    )
    .result(Value::Int(1));
}

/// Applying f#size to a non-existent file returns false.
#[test]
fn set_size_nonexistent_fails() {
    expr!(
        "f = file(\"nonexistent_xyz_set_size.txt\");
ok = f.set_file_size(0l);
if !ok { 1 } else { 0 }"
    )
    .result(Value::Int(1));
}

/// After truncation via f#size, a new write reopens the file and appends from the start.
#[test]
fn set_size_then_write() {
    expr!(
        "delete(\"test_sz_tw.txt\");
f = file(\"test_sz_tw.txt\");
f += \"Hello World\";
f#size = 0l;
g = file(\"test_sz_tw.txt\");
g += \"Hi\";
h = file(\"test_sz_tw.txt\");
result = h#read(100) as text;
delete(\"test_sz_tw.txt\");
result"
    )
    .result(Value::str("Hi"));
}
