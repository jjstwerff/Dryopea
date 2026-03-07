// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later
//
// Generate doc/stdlib.html from the documented default/*.lav standard library files.
// Run with: cargo run --bin gendoc

use dryopea::documentation::generate_docs;
use std::collections::HashMap;
use std::fs;

// ---  Data model  ---

#[derive(Debug)]
enum Entry {
    /// A named section (// --- Name ---).
    Section(String),
    /// A public item or section description.
    /// sig is empty for section-description items.
    Item { sig: String, doc: Vec<String> },
}

// ---  Entry point  ---

fn main() -> std::io::Result<()> {
    generate_docs()?;
    let files = [
        "default/01_code.lav",
        "default/02_images.lav",
        "default/03_text.lav",
    ];

    let mut entries: Vec<Entry> = Vec::new();
    for path in &files {
        match fs::read_to_string(path) {
            Ok(content) => parse_lav(&content, &mut entries),
            Err(e) => eprintln!("Cannot read {path}: {e}"),
        }
    }

    let html = render_html(&entries);
    fs::create_dir_all("doc").unwrap();
    fs::write("doc/stdlib.html", &html).unwrap();
    println!("Generated doc/stdlib.html ({} bytes)", html.len());

    link_doc_files(&entries);
    Ok(())
}

// ---  Parser  ---

fn parse_lav(content: &str, entries: &mut Vec<Entry>) {
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;
    let mut doc: Vec<String> = Vec::new();
    // Set after a section header so that the next comment block becomes the
    // section description rather than a per-item doc comment.
    let mut after_section = false;

    while i < lines.len() {
        let trimmed = lines[i].trim();

        // Section header: // --- Name ---
        if let Some(name) = parse_section(trimmed) {
            entries.push(Entry::Section(name));
            doc.clear();
            after_section = true;
            i += 1;
            continue;
        }

        // Comment line: accumulate into doc buffer
        if trimmed.starts_with("//") {
            let text = trimmed.trim_start_matches('/').trim().to_string();
            doc.push(text);
            i += 1;
            continue;
        }

        // Public declaration — always collected regardless of doc
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

        // Non-pub fn with a doc comment: user-visible functions like print/println/assert/panic.
        // Internal operator functions (fn Op...) are excluded.
        if trimmed.starts_with("fn ") && !trimmed.starts_with("fn Op") && !doc.is_empty() {
            let (sig, consumed) = collect_sig(&lines[i..]);
            entries.push(Entry::Item {
                sig,
                doc: std::mem::take(&mut doc),
            });
            after_section = false;
            i += consumed;
            continue;
        }

        // Anything else (blank lines, code, #rust attributes):
        // - #rust lines never disturb the doc buffer
        // - everything else triggers a flush:
        //   * right after a section header → emit as section description
        //   * otherwise → discard
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

/// Extract the section name from a `// --- Name ---` line, or return None.
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

/// Collect a complete declaration starting at lines[0].
/// Returns (signature_string, number_of_lines_consumed).
fn collect_sig(lines: &[&str]) -> (String, usize) {
    let first = lines[0].trim();
    // Structs and enums: keep the full body with fields/variants.
    if first.starts_with("pub struct") || first.starts_with("pub enum") {
        return collect_block(lines);
    }
    // Functions and other declarations: strip the body, show only the signature.
    (strip_body(first), 1)
}

/// Collect a brace-delimited block (struct or enum), stripping implementation-detail
/// comments such as byte-offset annotations (`// 8`).
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

/// Strip a trailing `// <integer>` byte-offset annotation from a line,
/// preserving all other leading whitespace and content.
fn strip_offset_comment(s: &str) -> String {
    if let Some(pos) = s.rfind("//")
        && s[pos + 2..].trim().parse::<i64>().is_ok()
    {
        return s[..pos].trim_end().to_string();
    }
    s.trim_end().to_string()
}

/// Strip a function body `{ ... }` from a single-line declaration,
/// respecting parenthesis depth so inner braces in type arguments are not confused
/// with the body delimiter.
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
            // Stop at `{` or `;` only when outside any parenthesised list
            '{' | ';' if depth == 0 => break,
            _ => result.push(ch),
        }
    }
    result.trim_end().to_string()
}

// ---  HTML renderer  ---

type DocItem = (String, Vec<String>); // (signature, doc lines)
type DocSection = (String, Vec<DocItem>); // (section name, items)

fn render_html(entries: &[Entry]) -> String {
    // Group items by section, merging any sections that share the same name
    // (e.g. the Text section appears in both 01_code.lav and 03_text.lav).
    let mut sections: Vec<DocSection> = Vec::new();
    let mut cur: Option<usize> = None;

    for entry in entries {
        match entry {
            Entry::Section(name) => {
                if let Some(idx) = sections.iter().position(|(n, _)| n == name) {
                    cur = Some(idx);
                } else {
                    sections.push((name.clone(), Vec::new()));
                    cur = Some(sections.len() - 1);
                }
            }
            Entry::Item { sig, doc } => {
                if let Some(idx) = cur {
                    sections[idx].1.push((sig.clone(), doc.clone()));
                }
            }
        }
    }

    let mut html = html_head();

    // Sidebar table of contents
    html.push_str("<nav id=\"toc\">\n");
    html.push_str("<p class=\"toc-title\">Lav Standard Library</p>\n<ul>\n");
    for (name, _) in &sections {
        html.push_str(&format!(
            "<li><a href=\"#{}\">{}</a></li>\n",
            section_id(name),
            esc(name)
        ));
    }
    html.push_str("</ul>\n</nav>\n");

    // Main content
    html.push_str("<main>\n<h1>Lav Standard Library</h1>\n");

    for (name, items) in &sections {
        html.push_str(&format!(
            "<section id=\"{}\">\n<h2>{}</h2>\n",
            section_id(name),
            esc(name)
        ));

        for (sig, doc_lines) in items {
            let paras = group_paragraphs(doc_lines);
            if sig.is_empty() {
                // Section description (no code block)
                html.push_str("<div class=\"section-desc\">");
                for p in &paras {
                    html.push_str(&format!("<p>{}</p>", esc(p)));
                }
                html.push_str("</div>\n");
            } else {
                html.push_str("<div class=\"item\">\n");
                html.push_str(&format!("<pre><code>{}</code></pre>\n", esc(sig)));
                for p in &paras {
                    html.push_str(&format!("<p>{}</p>\n", esc(p)));
                }
                html.push_str("</div>\n");
            }
        }

        html.push_str("</section>\n");
    }

    html.push_str("</main>\n</body>\n</html>\n");
    html
}

/// Convert a section name to a safe HTML anchor id.
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
        // Collapse multiple consecutive hyphens
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

/// Escape characters that have special meaning in HTML.
fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Join consecutive non-empty lines into a single paragraph string.
/// Blank lines create a new paragraph.
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

// ---  HTML boilerplate  ---

fn html_head() -> String {
    format!(
        "<!DOCTYPE html>\n\
         <html lang=\"en\">\n\
         <head>\n\
         <meta charset=\"UTF-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
         <title>Lav Standard Library</title>\n\
         <style>\n{CSS}</style>\n\
         </head>\n\
         <body>\n"
    )
}

// ---  Link map  ---

/// Maps stdlib item names to the section anchor they live in, split by the
/// span class used in the doc HTML files:
///   fn_map  → class="bi" and class="fn-call"  (functions)
///   ty_map  → class="ty"                       (primitive types)
///   en_map  → class="en"                       (structs, enums, constants)
struct LinkMaps {
    fn_map: HashMap<String, String>,
    ty_map: HashMap<String, String>,
    en_map: HashMap<String, String>,
}

fn build_link_maps(entries: &[Entry]) -> LinkMaps {
    let mut fn_map: HashMap<String, String> = HashMap::new();
    let mut ty_map: HashMap<String, String> = HashMap::new();
    let mut en_map: HashMap<String, String> = HashMap::new();

    // Track first-seen section id for each section name (mirrors render_html merging).
    let mut section_first: HashMap<String, String> = HashMap::new();
    let mut cur_section = String::new();

    for entry in entries {
        match entry {
            Entry::Section(name) => {
                let id = section_id(name);
                cur_section = section_first
                    .entry(name.clone())
                    .or_insert_with(|| id)
                    .clone();
            }
            Entry::Item { sig, .. } if !sig.is_empty() => {
                if let Some((name, kind)) = parse_sig_name(sig) {
                    let sec = cur_section.clone();
                    match kind {
                        "fn" => {
                            fn_map.entry(name).or_insert(sec);
                        }
                        "type" => {
                            ty_map.entry(name).or_insert(sec);
                        }
                        "struct" | "enum" | "const" => {
                            en_map.entry(name).or_insert(sec);
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    // `vector` is declared as `type vector` (not pub) but it is user-visible;
    // add it manually so class="ty">vector</span> links to the Collections section.
    ty_map
        .entry("vector".into())
        .or_insert_with(|| "collections".into());
    ty_map
        .entry("sorted".into())
        .or_insert_with(|| "collections".into());

    LinkMaps {
        fn_map,
        ty_map,
        en_map,
    }
}

/// Extract the identifier name and kind from a signature string.
/// Returns None for anonymous or unrecognised signatures.
fn parse_sig_name(sig: &str) -> Option<(String, &'static str)> {
    let s = sig.trim();
    let (kind, rest) = if let Some(r) = s.strip_prefix("pub fn ") {
        ("fn", r)
    } else if let Some(r) = s.strip_prefix("fn ") {
        ("fn", r)
    } else if let Some(r) = s.strip_prefix("pub type ") {
        ("type", r)
    } else if let Some(r) = s.strip_prefix("pub struct ") {
        ("struct", r)
    } else if let Some(r) = s.strip_prefix("pub enum ") {
        ("enum", r)
    } else if let Some(r) = s.strip_prefix("pub ") {
        // Constants: `pub PI = ...`, `pub E = ...`
        ("const", r)
    } else {
        return None;
    };

    let name: String = rest
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();

    if name.is_empty() {
        None
    } else {
        Some((name, kind))
    }
}

// ---  Doc HTML post-processing  ---

/// Inject stdlib links into every doc HTML file (except stdlib.html itself).
fn link_doc_files(entries: &[Entry]) {
    let maps = build_link_maps(entries);

    let dir = match fs::read_dir("doc") {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Cannot read doc/: {e}");
            return;
        }
    };

    let mut paths: Vec<_> = dir
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.extension().and_then(|e| e.to_str()) == Some("html")
                && p.file_name().and_then(|n| n.to_str()) != Some("stdlib.html")
        })
        .collect();
    paths.sort();

    let mut count = 0usize;
    for path in &paths {
        let original = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Cannot read {}: {e}", path.display());
                continue;
            }
        };
        let linked = apply_links(&original, &maps);
        if linked != original {
            if let Err(e) = fs::write(path, &linked) {
                eprintln!("Cannot write {}: {e}", path.display());
            } else {
                count += 1;
            }
        }
    }
    println!(
        "Linked stdlib references in {count}/{} doc files",
        paths.len()
    );
}

/// Replace unlinked `<span class="CLASS">NAME</span>` patterns with links to
/// the stdlib.html anchor for that name.  Idempotent: already-linked spans are
/// left untouched because the inner content no longer matches a plain name.
fn apply_links(html: &str, maps: &LinkMaps) -> String {
    let mut out = html.to_string();

    // Functions: span classes "bi" and "fn-call"
    for (name, sec) in &maps.fn_map {
        let href = format!("stdlib.html#{sec}");
        for class in &["bi", "fn-call"] {
            let old = format!("<span class=\"{class}\">{name}</span>");
            let new = format!("<span class=\"{class}\"><a href=\"{href}\">{name}</a></span>");
            out = out.replace(&old, &new);
        }
    }

    // Primitive types: span class "ty"
    for (name, sec) in &maps.ty_map {
        let href = format!("stdlib.html#{sec}");
        let old = format!("<span class=\"ty\">{name}</span>");
        let new = format!("<span class=\"ty\"><a href=\"{href}\">{name}</a></span>");
        out = out.replace(&old, &new);
    }

    // Structs, enums, constants: span class "en"
    for (name, sec) in &maps.en_map {
        let href = format!("stdlib.html#{sec}");
        let old = format!("<span class=\"en\">{name}</span>");
        let new = format!("<span class=\"en\"><a href=\"{href}\">{name}</a></span>");
        out = out.replace(&old, &new);
    }

    out
}

// ---  HTML boilerplate  ---

const CSS: &str = "\
* { box-sizing: border-box; margin: 0; padding: 0; }\n\
\n\
body {\n\
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;\n\
    font-size: 16px;\n\
    line-height: 1.6;\n\
    color: #24292e;\n\
    display: flex;\n\
    min-height: 100vh;\n\
    background: #fff;\n\
}\n\
\n\
/* ---- Sidebar ---- */\n\
#toc {\n\
    width: 230px;\n\
    flex-shrink: 0;\n\
    background: #1a1b26;\n\
    padding: 1.5rem 0;\n\
    position: sticky;\n\
    top: 0;\n\
    height: 100vh;\n\
    overflow-y: auto;\n\
}\n\
\n\
.toc-title {\n\
    font-size: 0.7rem;\n\
    font-weight: 700;\n\
    text-transform: uppercase;\n\
    letter-spacing: 0.12em;\n\
    color: #7aa2f7;\n\
    padding: 0 1.25rem 0.75rem;\n\
    margin-bottom: 0.4rem;\n\
    border-bottom: 1px solid #2a2b3d;\n\
}\n\
\n\
#toc ul { list-style: none; padding: 0.25rem 0; }\n\
\n\
#toc a {\n\
    display: block;\n\
    padding: 0.3rem 1.25rem;\n\
    color: #a9b1d6;\n\
    text-decoration: none;\n\
    font-size: 0.875rem;\n\
    border-left: 3px solid transparent;\n\
}\n\
\n\
#toc a:hover {\n\
    color: #c0caf5;\n\
    background: #1e2030;\n\
    border-left-color: #7aa2f7;\n\
}\n\
\n\
/* ---- Main content ---- */\n\
main {\n\
    flex: 1;\n\
    padding: 2.5rem 3rem;\n\
    max-width: 860px;\n\
    overflow-x: hidden;\n\
}\n\
\n\
h1 {\n\
    font-size: 2rem;\n\
    font-weight: 700;\n\
    color: #1a1b26;\n\
    margin-bottom: 2.5rem;\n\
    padding-bottom: 0.75rem;\n\
    border-bottom: 2px solid #e1e4e8;\n\
}\n\
\n\
/* ---- Sections ---- */\n\
section { margin-bottom: 3rem; }\n\
\n\
section h2 {\n\
    font-size: 1.3rem;\n\
    font-weight: 600;\n\
    color: #1a1b26;\n\
    padding-bottom: 0.4rem;\n\
    margin-bottom: 1rem;\n\
    border-bottom: 2px solid #7aa2f7;\n\
}\n\
\n\
.section-desc {\n\
    margin-bottom: 1.25rem;\n\
}\n\
\n\
.section-desc p {\n\
    color: #57606a;\n\
    font-size: 0.95rem;\n\
    line-height: 1.7;\n\
}\n\
\n\
/* ---- Items ---- */\n\
.item {\n\
    padding: 0.9rem 0;\n\
    border-bottom: 1px solid #eaecef;\n\
}\n\
\n\
.item:last-child { border-bottom: none; }\n\
\n\
pre {\n\
    background: #f6f8fa;\n\
    border: 1px solid #e1e4e8;\n\
    border-radius: 6px;\n\
    padding: 0.65rem 1rem;\n\
    overflow-x: auto;\n\
    margin-bottom: 0.45rem;\n\
}\n\
\n\
code {\n\
    font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', 'Consolas', monospace;\n\
    font-size: 0.84rem;\n\
    color: #24292e;\n\
    white-space: pre;\n\
}\n\
\n\
.item p {\n\
    color: #444d56;\n\
    font-size: 0.925rem;\n\
    line-height: 1.65;\n\
    margin-top: 0.2rem;\n\
}\n\
";
