// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//
// Generate standard library HTML pages from the documented default/*.loft files.
// Run with: cargo run --bin gendoc

use dryopea::documentation::{
    StdlibSection, build_nav, gather_topic_info, generate_docs, page_html,
};
use std::collections::HashMap;
use std::fs;

// ---  Data model  ---

#[derive(Debug)]
enum Entry {
    /// A named section header (// --- Name ---).
    Section(String),
    /// A public item or section description.
    /// sig is empty for section-description items.
    Item { sig: String, doc: Vec<String> },
}

struct SectionFull {
    id: String,
    name: String,
    items: Vec<(String, Vec<String>)>, // (signature, doc_lines)
}

// ---  Entry point  ---

fn main() -> std::io::Result<()> {
    let files = [
        "default/01_code.loft",
        "default/02_images.loft",
        "default/03_text.loft",
    ];

    let mut entries: Vec<Entry> = Vec::new();
    for path in &files {
        match fs::read_to_string(path) {
            Ok(content) => parse_loft(&content, &mut entries),
            Err(e) => eprintln!("Cannot read {path}: {e}"),
        }
    }

    let sections = build_sections(&entries);
    let link_map = build_link_map(&sections);

    let stdlib_info: Vec<StdlibSection> = sections
        .iter()
        .map(|s| StdlibSection {
            id: s.id.clone(),
            name: s.name.clone(),
        })
        .collect();

    generate_docs(&stdlib_info, &link_map)?;

    let topic_info = gather_topic_info();

    for section in &sections {
        generate_stdlib_section(section, &stdlib_info, &topic_info)?;
    }

    generate_stdlib_toc(&sections, &stdlib_info, &topic_info)?;
    generate_search_index(&sections, &stdlib_info)?;

    println!("Generated {} stdlib section pages", sections.len());
    Ok(())
}

// ---  Parser  ---

fn parse_loft(content: &str, entries: &mut Vec<Entry>) {
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;
    let mut doc: Vec<String> = Vec::new();
    let mut after_section = false;

    while i < lines.len() {
        let trimmed = lines[i].trim();

        if let Some(name) = parse_section(trimmed) {
            entries.push(Entry::Section(name));
            doc.clear();
            after_section = true;
            i += 1;
            continue;
        }

        if trimmed.starts_with("//") {
            let text = trimmed.trim_start_matches('/').trim().to_string();
            doc.push(text);
            i += 1;
            continue;
        }

        if trimmed.starts_with("pub ") {
            let (sig, consumed) = collect_sig(&lines[i..]);
            entries.push(Entry::Item {
                sig,
                doc: std::mem::take(&mut doc),
            });
            after_section = false;
            i += consumed;
            continue;
        }

        // #rust attribute lines do not break doc accumulation.
        if !trimmed.starts_with('#') {
            if after_section && !doc.is_empty() {
                entries.push(Entry::Item {
                    sig: String::new(),
                    doc: std::mem::take(&mut doc),
                });
            } else {
                doc.clear();
            }
            after_section = false;
        }

        i += 1;
    }
}

fn parse_section(trimmed: &str) -> Option<String> {
    if !trimmed.starts_with("// ---") {
        return None;
    }
    let inner = trimmed
        .trim_start_matches('/')
        .trim()
        .trim_matches('-')
        .trim();
    if inner.is_empty() {
        None
    } else {
        Some(inner.to_string())
    }
}

/// Use this rather than `collect_block` directly; it selects between
/// signature-only and full-body capture based on the declaration kind.
fn collect_sig(lines: &[&str]) -> (String, usize) {
    let first = lines[0].trim();
    if first.starts_with("pub struct") || first.starts_with("pub enum") {
        return collect_block(lines);
    }
    (strip_body(first), 1)
}

fn collect_block(lines: &[&str]) -> (String, usize) {
    let mut result: Vec<String> = Vec::new();
    let mut depth = 0i32;
    for (idx, &line) in lines.iter().enumerate() {
        for ch in line.trim().chars() {
            match ch {
                '{' => depth += 1,
                '}' => depth -= 1,
                _ => {}
            }
        }
        result.push(strip_offset_comment(line));
        if depth == 0 && idx > 0 {
            return (result.join("\n"), idx + 1);
        }
    }
    (result.join("\n"), lines.len())
}

fn strip_offset_comment(s: &str) -> String {
    if let Some(pos) = s.rfind("//")
        && s[pos + 2..].trim().parse::<i64>().is_ok()
    {
        return s[..pos].trim_end().to_string();
    }
    s.trim_end().to_string()
}

fn strip_body(sig: &str) -> String {
    let mut depth = 0i32;
    let mut result = String::new();
    for ch in sig.chars() {
        match ch {
            '(' => {
                depth += 1;
                result.push(ch);
            }
            ')' => {
                depth -= 1;
                result.push(ch);
            }
            '{' | ';' if depth == 0 => break,
            _ => result.push(ch),
        }
    }
    result.trim_end().to_string()
}

// ---  Section grouping  ---

/// Build this before generating HTML or the link map; it groups the flat entry
/// list into the structure both renderers need, merging sections that share a
/// name across multiple source files.
fn build_sections(entries: &[Entry]) -> Vec<SectionFull> {
    let mut sections: Vec<SectionFull> = Vec::new();
    let mut section_idx: Option<usize> = None;

    for entry in entries {
        match entry {
            Entry::Section(name) => {
                if let Some(idx) = sections.iter().position(|s| &s.name == name) {
                    section_idx = Some(idx);
                } else {
                    sections.push(SectionFull {
                        id: section_id(name),
                        name: name.clone(),
                        items: Vec::new(),
                    });
                    section_idx = Some(sections.len() - 1);
                }
            }
            Entry::Item { sig, doc } => {
                if let Some(idx) = section_idx {
                    sections[idx].items.push((sig.clone(), doc.clone()));
                }
            }
        }
    }
    sections
}

// ---  Link map  ---

/// Build this before calling `generate_docs` and `generate_stdlib_section`;
/// the map lets the syntax highlighter inject stdlib links without a second
/// pass over the generated HTML.
fn build_link_map(sections: &[SectionFull]) -> HashMap<String, String> {
    let mut map: HashMap<String, String> = HashMap::new();

    for section in sections {
        let url = format!("stdlib-{}.html", section.id);
        for (sig, _) in &section.items {
            if sig.is_empty() {
                continue;
            }
            if let Some(name) = sig_name(sig) {
                map.entry(name).or_insert_with(|| url.clone());
            }
        }
    }

    // vector and sorted are user-visible type aliases not captured by pub declarations.
    map.entry("vector".into())
        .or_insert_with(|| "stdlib-collections.html".into());
    map.entry("sorted".into())
        .or_insert_with(|| "stdlib-collections.html".into());

    map
}

fn sig_name(sig: &str) -> Option<String> {
    let trimmed = sig.trim();
    let rest = trimmed
        .strip_prefix("pub fn ")
        .or_else(|| trimmed.strip_prefix("fn "))
        .or_else(|| trimmed.strip_prefix("pub type "))
        .or_else(|| trimmed.strip_prefix("pub struct "))
        .or_else(|| trimmed.strip_prefix("pub enum "))
        .or_else(|| trimmed.strip_prefix("pub "))?;
    let name: String = rest
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();
    if name.is_empty() { None } else { Some(name) }
}

// ---  HTML rendering  ---

fn section_id(name: &str) -> String {
    name.to_lowercase()
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '-' {
                c
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn group_paragraphs(lines: &[String]) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    for line in lines {
        if line.is_empty() {
            if !current.is_empty() {
                result.push(current.trim().to_string());
                current = String::new();
            }
        } else {
            if !current.is_empty() {
                current.push(' ');
            }
            current.push_str(line);
        }
    }
    if !current.is_empty() {
        result.push(current.trim().to_string());
    }
    result
}

/// Call once per section after building the link map; generates a self-contained
/// page with full nav so readers can navigate to any other section or topic.
fn generate_stdlib_section(
    section: &SectionFull,
    stdlib_info: &[StdlibSection],
    topic_info: &[(String, String)],
) -> std::io::Result<()> {
    let stem = format!("stdlib-{}", section.id);
    let nav = build_nav(topic_info, stdlib_info, &stem);
    let mut body = String::new();
    for (sig, doc_lines) in &section.items {
        let paras = group_paragraphs(doc_lines);
        if sig.is_empty() {
            body.push_str("<div class=\"section-desc\">");
            for p in &paras {
                body.push_str(&format!("<p>{}</p>", esc(p)));
            }
            body.push_str("</div>\n");
        } else {
            body.push_str("<div class=\"item\">\n");
            body.push_str(&format!("<pre><code>{}</code></pre>\n", esc(sig)));
            for p in &paras {
                body.push_str(&format!("<p>{}</p>\n", esc(p)));
            }
            body.push_str("</div>\n");
        }
    }
    let html = page_html(&section.name, &nav, &section.name, &body);
    fs::create_dir_all("doc")?;
    fs::write(format!("doc/{stem}.html"), html)?;
    println!("Generated doc/{stem}.html");
    Ok(())
}

/// Generate after all section pages exist so the item counts are accurate and
/// readers get a complete overview when landing on the stdlib index.
fn generate_stdlib_toc(
    sections: &[SectionFull],
    stdlib_info: &[StdlibSection],
    topic_info: &[(String, String)],
) -> std::io::Result<()> {
    let nav = build_nav(topic_info, stdlib_info, "stdlib");
    let mut body = String::new();
    body.push_str("<div class=\"grid\">\n");
    for section in sections {
        let count = section.items.iter().filter(|(s, _)| !s.is_empty()).count();
        body.push_str(&format!(
            "  <a class=\"card\" href=\"stdlib-{id}.html\"><h2>{name}</h2><p>{count} items</p></a>\n",
            id = section.id,
            name = esc(&section.name),
        ));
    }
    body.push_str("</div>\n");
    let html = page_html("Standard Library", &nav, "Lav Standard Library", &body);
    fs::write("doc/stdlib.html", &html)?;
    println!("Generated doc/stdlib.html");
    Ok(())
}

/// Generate last so all section URLs are stable before they are written into
/// the index; the index is consumed at page load so stale URLs cause silent
/// broken links.
fn generate_search_index(
    sections: &[SectionFull],
    stdlib_info: &[StdlibSection],
) -> std::io::Result<()> {
    let mut entries: Vec<String> = Vec::new();

    for sec in stdlib_info {
        entries.push(format!(
            "{{name:{:?},kind:\"section\",url:\"stdlib-{}.html\"}}",
            sec.name, sec.id
        ));
    }

    for section in sections {
        let url = format!("stdlib-{}.html", section.id);
        for (sig, _) in &section.items {
            if sig.is_empty() {
                continue;
            }
            if let Some(name) = sig_name(sig) {
                let kind = sig_kind(sig);
                entries.push(format!("{{name:{:?},kind:{:?},url:{:?}}}", name, kind, url));
            }
        }
    }

    let js = format!("const SEARCH_INDEX=[\n{}\n];\n", entries.join(",\n"));
    fs::write("doc/search-index.js", js)?;
    println!("Generated doc/search-index.js ({} entries)", entries.len());
    Ok(())
}

fn sig_kind(sig: &str) -> &'static str {
    let trimmed = sig.trim();
    if trimmed.starts_with("pub fn ") || trimmed.starts_with("fn ") {
        "fn"
    } else if trimmed.starts_with("pub type ") {
        "type"
    } else if trimmed.starts_with("pub struct ") {
        "struct"
    } else if trimmed.starts_with("pub enum ") {
        "enum"
    } else {
        "const"
    }
}
