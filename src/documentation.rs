// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

use std::collections::HashMap;
use std::fmt::Write;
use std::fs::{DirEntry, File};
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

/// A stdlib section visible in the nav and the search index.
pub struct StdlibSection {
    pub id: String,   // URL-safe slug, e.g. "output-and-diagnostics"
    pub name: String, // Human-readable label, e.g. "Output and diagnostics"
}

struct Topic {
    file: PathBuf,
    filename: String, // Stem without extension, e.g. "05-float"
    name: String,     // Short display name from @NAME header
    title: String,    // Descriptive title from @TITLE header
}

#[must_use]
fn gather_topics() -> Vec<Topic> {
    let mut result: Vec<Topic> = Vec::new();
    let dir = std::fs::read_dir("tests/suite").unwrap();
    let mut entries: Vec<_> = dir.filter_map(Result::ok).collect();
    entries.sort_by_key(DirEntry::file_name);
    entries
        .iter()
        .map(DirEntry::path)
        .filter(|p| {
            p.extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("loft"))
        })
        .for_each(|file| {
            let file_handle = File::open(file.clone()).expect("failed to open file");
            let reader = BufReader::new(file_handle);
            let filename = file.file_stem().unwrap().to_string_lossy().into_owned();
            let mut name = String::new();
            let mut title = String::new();
            reader.lines().for_each(|line_result| {
                if let Ok(line) = line_result {
                    if let Some(s) = line.strip_prefix("// @NAME: ") {
                        name = s.to_string();
                    }
                    if let Some(s) = line.strip_prefix("// @TITLE: ") {
                        title = s.to_string();
                    }
                }
            });
            result.push(Topic {
                file,
                filename,
                name,
                title,
            });
        });
    result
}

/// Call this after building the stdlib sections and link map so that language
/// pages are generated with stdlib links already inlined and their nav matches
/// the stdlib pages generated separately.
/// # Errors
/// When the `doc/` directory is unwritable.
pub fn generate_docs<S: std::hash::BuildHasher>(
    stdlib_sections: &[StdlibSection],
    link_map: &HashMap<String, String, S>,
) -> std::io::Result<()> {
    let topics = gather_topics();
    write_index(&topics, stdlib_sections)?;
    let nav_info = topic_nav_info(&topics);
    for entry in &topics {
        if entry.filename.starts_with("00-") {
            continue;
        }
        let stem = entry
            .file
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();
        if let Ok(source) = std::fs::read_to_string(&entry.file) {
            let html = render_doc_page(
                &source,
                &entry.name,
                &stem,
                &nav_info,
                stdlib_sections,
                link_map,
            );
            std::fs::write(format!("doc/{stem}.html"), html)?;
        }
    }
    Ok(())
}

fn index_intro(topic: &Topic) -> std::io::Result<String> {
    let mut result = String::new();
    let file = File::open(&topic.file).expect("failed to open file");
    let source = BufReader::new(file);
    let mut in_header = true;
    let mut in_list = false;
    for line_result in source.lines() {
        let line = line_result?;
        let trimmed = line.trim();
        if in_header && skip_header(trimmed) {
            continue;
        }
        in_header = false;
        if trimmed.starts_with("//") {
            let text = trimmed.strip_prefix("//").unwrap_or("").trim().to_string();
            if let Some(n) = text.strip_prefix("- ") {
                if !in_list {
                    write!(result, "<ul>").expect("");
                    in_list = true;
                }
                writeln!(result, "<li>{n}</li>").expect("");
            } else {
                if in_list {
                    writeln!(result, "</ul>").expect("");
                    in_list = false;
                }
                writeln!(result, "<p>{text}</p>").expect("");
            }
        }
    }
    if in_list {
        writeln!(result, "</ul>").expect("");
    }
    Ok(result)
}

fn skip_header(trimmed: &str) -> bool {
    trimmed.starts_with("// Copyright")
        || trimmed.starts_with("// SPDX")
        || trimmed.starts_with("// @NAME: ")
        || trimmed.starts_with("// @TITLE: ")
        || trimmed.is_empty()
}

fn write_index(topics: &[Topic], stdlib_sections: &[StdlibSection]) -> std::io::Result<()> {
    let lang_cards: String = topics.iter().fold(String::new(), |mut output, topic| {
        if !topic.filename.starts_with("00-") {
            let _ = writeln!(
                output,
                "    <a class=\"card\" href=\"{}.html\"><h2>{}</h2><p>{}</p></a>",
                topic.filename, topic.name, topic.title
            );
        }
        output
    });
    let lib_cards: String = stdlib_sections
        .iter()
        .fold(String::new(), |mut output, sec| {
            let _ = writeln!(
                output,
                "    <a class=\"card\" href=\"stdlib-{}.html\"><h2>{}</h2></a>",
                sec.id, sec.name
            );
            output
        });
    let title = topics[0].title.clone();
    let intro = index_intro(&topics[0])?;
    let html = format!(
        "<!DOCTYPE html>\n\
<html lang=\"en\">\n\
<head>\n\
  <meta charset=\"utf-8\">\n\
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
  <title>Loft Language</title>\n\
  <link rel=\"stylesheet\" href=\"style.css\">\n\
</head>\n\
<body>\n\
  <header>\n\
    <h1>Loft</h1>\n\
    <p class=\"tagline\">{title}</p>\n\
  </header>\n\
  <section class=\"intro\">\n\
{intro}\
  </section>\n\
  <section class=\"topics\">\n\
    <h2 class=\"topics-heading\">Language</h2>\n\
    <div class=\"grid\">\n\
{lang_cards}\
    </div>\n\
  </section>\n\
  <section class=\"topics\">\n\
    <h2 class=\"topics-heading\">Standard Library</h2>\n\
    <div class=\"grid\">\n\
{lib_cards}\
    </div>\n\
  </section>\n\
  <script src=\"search-index.js\"></script>\n\
  <script src=\"search.js\"></script>\n\
</body>\n\
</html>\n"
    );
    std::fs::write("doc/index.html", html)
}

fn html_esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

// ─── Section parser ───────────────────────────────────────────────────────────

enum DocSection {
    /// Consecutive `//` comment lines with the `// ` prefix stripped.
    Prose(Vec<String>),
    /// Consecutive non-comment source lines kept verbatim.
    Code(Vec<String>),
}

fn parse_sections(source: &str) -> Vec<DocSection> {
    let mut sections: Vec<DocSection> = Vec::new();
    let mut prose: Vec<String> = Vec::new();
    let mut code: Vec<String> = Vec::new();
    let mut in_header = true;

    for line in source.lines() {
        let trimmed = line.trim();
        if in_header && skip_header(trimmed) {
            continue;
        }
        in_header = false;

        if trimmed.starts_with("//") {
            if !code.is_empty() {
                sections.push(DocSection::Code(std::mem::take(&mut code)));
            }
            let text = trimmed.strip_prefix("//").unwrap_or("").trim().to_string();
            prose.push(text);
        } else if trimmed.is_empty() {
            if !prose.is_empty() {
                sections.push(DocSection::Prose(std::mem::take(&mut prose)));
            }
            if !code.is_empty() {
                sections.push(DocSection::Code(std::mem::take(&mut code)));
            }
        } else {
            if !prose.is_empty() {
                sections.push(DocSection::Prose(std::mem::take(&mut prose)));
            }
            code.push(line.to_string());
        }
    }
    if !prose.is_empty() {
        sections.push(DocSection::Prose(prose));
    }
    if !code.is_empty() {
        sections.push(DocSection::Code(code));
    }
    sections
}

// ─── Syntax highlighter ───────────────────────────────────────────────────────

const KW: &[&str] = &[
    "fn", "if", "else", "for", "in", "return", "break", "continue", "struct", "enum", "pub", "use",
    "type", "as", "not", "null", "true", "false", "and", "or", "limit", "default", "virtual",
];
const TY: &[&str] = &[
    "integer",
    "text",
    "boolean",
    "float",
    "single",
    "long",
    "character",
    "vector",
    "sorted",
    "index",
    "hash",
    "reference",
    "u8",
    "u16",
    "u32",
    "i8",
    "i16",
    "i32",
    "i64",
];
const BI: &[&str] = &[
    "assert", "panic", "len", "round", "ceil", "floor", "abs", "sin", "cos", "log", "rev", "file",
    "min", "max", "sqrt", "typeof", "typedef",
];

fn scan_quoted(chars: &[char], i: usize, n: usize, delim: char) -> usize {
    let mut j = i + 1;
    while j < n && chars[j] != delim {
        j += 1;
    }
    if j < n { j + 1 } else { j }
}

fn scan_number(chars: &[char], i: usize, n: usize) -> usize {
    let mut j = i;
    if chars[i] == '0' && i + 1 < n {
        match chars[i + 1] {
            'x' | 'X' => {
                j += 2;
                while j < n && chars[j].is_ascii_hexdigit() {
                    j += 1;
                }
            }
            'b' | 'B' => {
                j += 2;
                while j < n && (chars[j] == '0' || chars[j] == '1') {
                    j += 1;
                }
            }
            'o' | 'O' => {
                j += 2;
                while j < n && chars[j].is_ascii_digit() {
                    j += 1;
                }
            }
            _ => {
                while j < n && (chars[j].is_ascii_digit() || chars[j] == '.' || chars[j] == '_') {
                    j += 1;
                }
            }
        }
    } else {
        while j < n && (chars[j].is_ascii_digit() || chars[j] == '.' || chars[j] == '_') {
            j += 1;
        }
    }
    if j < n && (chars[j] == 'l' || chars[j] == 'f') {
        j += 1;
    }
    j
}

fn word_class(word: &str, is_call: bool) -> &'static str {
    if KW.contains(&word) {
        "kw"
    } else if TY.contains(&word) {
        "ty"
    } else if BI.contains(&word) {
        "bi"
    } else if word.starts_with(|c: char| c.is_uppercase()) {
        "en"
    } else if is_call {
        "fn-call"
    } else {
        ""
    }
}

fn emit_span<S: std::hash::BuildHasher>(
    out: &mut String,
    cls: &str,
    word: &str,
    link_map: &HashMap<String, String, S>,
) {
    out.push_str("<span class=\"");
    out.push_str(cls);
    out.push_str("\">");
    if let Some(url) = link_map.get(word) {
        out.push_str("<a href=\"");
        out.push_str(url);
        out.push_str("\">");
        out.push_str(&html_esc(word));
        out.push_str("</a>");
    } else {
        out.push_str(&html_esc(word));
    }
    out.push_str("</span>");
}

/// Use this instead of raw HTML concatenation for code blocks; stdlib links are
/// injected here so no separate post-processing pass over the HTML is needed.
fn highlight_loft<S: std::hash::BuildHasher>(
    code: &str,
    link_map: &HashMap<String, String, S>,
) -> String {
    let mut out = String::with_capacity(code.len() * 2);

    for line in code.lines() {
        let chars: Vec<char> = line.chars().collect();
        let char_count = chars.len();
        let mut pos = 0;

        while pos < char_count {
            if pos + 1 < char_count && chars[pos] == '/' && chars[pos + 1] == '/' {
                let rest: String = chars[pos..].iter().collect();
                out.push_str("<span class=\"cm\">");
                out.push_str(&html_esc(&rest));
                out.push_str("</span>");
                pos = char_count;
                continue;
            }

            if chars[pos] == '"' {
                let end = scan_quoted(&chars, pos, char_count, '"');
                let token: String = chars[pos..end].iter().collect();
                out.push_str("<span class=\"st\">");
                out.push_str(&html_esc(&token));
                out.push_str("</span>");
                pos = end;
                continue;
            }

            if chars[pos] == '\'' {
                let end = scan_quoted(&chars, pos, char_count, '\'');
                let token: String = chars[pos..end].iter().collect();
                out.push_str("<span class=\"ch\">");
                out.push_str(&html_esc(&token));
                out.push_str("</span>");
                pos = end;
                continue;
            }

            if chars[pos].is_ascii_digit() {
                let end = scan_number(&chars, pos, char_count);
                let token: String = chars[pos..end].iter().collect();
                out.push_str("<span class=\"nm\">");
                out.push_str(&html_esc(&token));
                out.push_str("</span>");
                pos = end;
                continue;
            }

            if chars[pos].is_alphabetic() || chars[pos] == '_' {
                let mut end = pos;
                while end < char_count && (chars[end].is_alphanumeric() || chars[end] == '_') {
                    end += 1;
                }
                let word: String = chars[pos..end].iter().collect();
                let mut peek = end;
                while peek < char_count && chars[peek] == ' ' {
                    peek += 1;
                }
                let cls = word_class(&word, peek < char_count && chars[peek] == '(');
                if cls.is_empty() {
                    out.push_str(&html_esc(&word));
                } else {
                    emit_span(&mut out, cls, &word, link_map);
                }
                pos = end;
                continue;
            }

            out.push_str(&html_esc(&chars[pos].to_string()));
            pos += 1;
        }
        out.push('\n');
    }

    if out.ends_with('\n') {
        out.pop();
    }
    out
}

// ─── Nav builder ──────────────────────────────────────────────────────────────

/// Shared transformation from a loaded topic list to (filename, name) pairs.
/// Separates the disk-reading concern in `gather_topic_info` from the filtering
/// logic used by `render_doc_page` on its already-loaded topic slice.
fn topic_nav_info(topics: &[Topic]) -> Vec<(String, String)> {
    topics
        .iter()
        .filter(|t| !t.filename.starts_with("00-"))
        .map(|t| (t.filename.clone(), t.name.clone()))
        .collect()
}

/// Use when building stdlib pages outside of `generate_docs`, where the full
/// `Topic` list is not already in scope.
#[must_use]
pub fn gather_topic_info() -> Vec<(String, String)> {
    topic_nav_info(&gather_topics())
}

/// Use to get consistent nav HTML for any page — language topic or stdlib
/// section — so that switching page types does not break the nav structure.
/// `active` is the filename stem of the current page.
#[must_use]
pub fn build_nav(
    topic_info: &[(String, String)],
    stdlib_sections: &[StdlibSection],
    active: &str,
) -> String {
    let mut parts: Vec<String> = Vec::new();

    parts.push("<a href=\"index.html\">Home</a>".to_string());
    parts.push("<span class=\"nav-sep\">Language:</span>".to_string());

    // vs-Rust is a hand-maintained page with no corresponding .loft file.
    if active == "00-vs-rust" {
        parts.push("<span class=\"cur\">vs Rust</span>".to_string());
    } else {
        parts.push("<a href=\"00-vs-rust.html\">vs Rust</a>".to_string());
    }

    for (filename, name) in topic_info {
        if filename == active {
            parts.push(format!("<span class=\"cur\">{name}</span>"));
        } else {
            parts.push(format!("<a href=\"{filename}.html\">{name}</a>"));
        }
    }

    if !stdlib_sections.is_empty() {
        parts.push("<span class=\"nav-sep\">Library:</span>".to_string());
        for sec in stdlib_sections {
            let stem = format!("stdlib-{}", sec.id);
            if stem == active {
                parts.push(format!("<span class=\"cur\">{}</span>", sec.name));
            } else {
                parts.push(format!(
                    "<a href=\"stdlib-{}.html\">{}</a>",
                    sec.id, sec.name
                ));
            }
        }
    }

    let links = parts.join(" · ");
    format!(
        "<div class=\"nav-links\">{links}</div>\
<div class=\"search-wrap\">\
<input type=\"search\" id=\"search\" placeholder=\"Search functions, types…\" autocomplete=\"off\">\
<div id=\"search-results\" class=\"search-results\" hidden></div>\
</div>"
    )
}

// ─── HTML page renderer ───────────────────────────────────────────────────────

/// Use to get consistent page structure for both language topic pages and stdlib
/// section pages; avoids duplicating the HTML boilerplate in two places.
#[must_use]
pub fn page_html(title: &str, nav: &str, h1: &str, body: &str) -> String {
    format!(
        "<!DOCTYPE html>\n\
<html lang=\"en\">\n\
<head>\n\
  <meta charset=\"utf-8\">\n\
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
  <title>Loft - {title}</title>\n\
  <link rel=\"stylesheet\" href=\"style.css\">\n\
</head>\n\
<body>\n\
  <nav>{nav}</nav>\n\
  <h1>{h1}</h1>\n\
  <article>\n{body}\n  </article>\n\
  <script src=\"search-index.js\"></script>\n\
  <script src=\"search.js\"></script>\n\
</body>\n\
</html>\n"
    )
}

fn render_doc_page<S: std::hash::BuildHasher>(
    source: &str,
    name: &str,
    active: &str,
    topic_info: &[(String, String)],
    stdlib_sections: &[StdlibSection],
    link_map: &HashMap<String, String, S>,
) -> String {
    let nav = build_nav(topic_info, stdlib_sections, active);
    let mut body = String::new();
    for section in parse_sections(source) {
        match section {
            DocSection::Prose(lines) => {
                let text = lines.join(" ");
                if !text.is_empty() {
                    body.push_str("<p>");
                    body.push_str(&html_esc(&text));
                    body.push_str("</p>\n");
                }
            }
            DocSection::Code(lines) => {
                let highlighted = highlight_loft(&lines.join("\n"), link_map);
                body.push_str("<pre><code>");
                body.push_str(&highlighted);
                body.push_str("</code></pre>\n");
            }
        }
    }
    page_html(name, &nav, name, &body)
}
