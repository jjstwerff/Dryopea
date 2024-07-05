// Copyright (c) 2024 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//! Opening png images inside a store
use crate::store::Store;
use png::Decoder;
use std::fs::File;

#[allow(dead_code)]
pub fn read(file_name: &str, store: &mut Store) -> std::io::Result<(u32, u32, u32)> {
    let decoder = Decoder::new(File::open(file_name)?);
    let mut reader = decoder.read_info()?;
    let img = store.claim((reader.output_buffer_size() / 8) as u32 + 1);
    let info = reader.next_frame(store.buffer(img)).unwrap();
    Ok((img, info.width, info.height))
}

#[test]
fn show_png() {
    let mut store = Store::new(12 + 256 * 256 * 3 / 8);
    let (img, _h, w) = read("example/map.png", &mut store).unwrap();
    for y in 0..128 {
        if y % 2 == 0 {
            continue;
        }
        for x in 0..128 {
            if store.get_byte(img, 8 + (x + y * w as isize) * 6, 0) > 0 {
                print!("x");
            } else if store.get_byte(img, 9 + (x + y * w as isize) * 6, 0) > 0 {
                print!("b");
            } else {
                print!(".");
            }
        }
        println!();
    }
}
