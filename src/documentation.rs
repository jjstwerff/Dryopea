// Copyright (c) 2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

use std::fmt::Write;
use std::fs::{DirEntry, File};
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

struct Topic {
    file: PathBuf, // The file inside the /doc/suite/ directory.
    filename: String,
    name: String,  // Short name of the topic.
    title: String, // Descriptive title of the topic.
}

/// One-line description for each numbered suite file, matched by a stem prefix.
/// Order mirrors the file numbering; unrecognized files get an empty description.
/// Look up the description for a file whose stem starts with `prefix` (e.g. "06").
#[must_use]
fn gather_topics() -> Vec<Topic> {
    let mut result: Vec<Topic> = Vec::new();
    let dir = std::fs::read_dir("tests/suite").unwrap();
    let mut entries: Vec<_> = dir.filter_map(Result::ok).collect();
    entries.sort_by_key(DirEntry::file_name);
    entries
        .iter()
        .map(DirEntry::path)
        .filter(|p| p.extension().is_some_and(|e| e.eq_ignore_ascii_case("lav")))
        .for_each(|file| {
            let f = File::open(file.clone()).expect("failed to open file");
            let reader = BufReader::new(f);
            let filename = file.file_stem().unwrap().to_string_lossy().into_owned();
            let mut name = String::new();
            let mut title = String::new();
            reader.lines().for_each(|l| {
                if let Ok(line) = l {
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

/// Render an HTML documentation page for every `.lav` suite file and write it
/// to `doc/<stem>.html`, then write `doc/index.html`.
/// Called unconditionally from `dir()` so docs are always up to date.
/// # Errors
/// When /doc directory is unwritable
pub fn generate_docs() -> std::io::Result<()> {
    let topics = gather_topics();
    write_index(&topics)?;
    for (idx, entry) in topics.iter().enumerate() {
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
            let html = render_doc_page(&source, &topics, idx);
            std::fs::write(format!("doc/{stem}.html"), html)?;
        }
    }
    Ok(())
}

fn index_intro(topic: &Topic) -> std::io::Result<String> {
    let mut result = String::new();
    let f = File::open(&topic.file).expect("failed to open file");
    let source = BufReader::new(f);
    let mut in_header = true;
    let mut in_list = false;
    for l in source.lines() {
        let line = l?;
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
    Ok(result)
}

fn skip_header(trimmed: &str) -> bool {
    trimmed.starts_with("// Copyright")
        || trimmed.starts_with("// SPDX")
        || trimmed.starts_with("// @NAME: ")
        || trimmed.starts_with("// @TITLE: ")
        || trimmed.is_empty()
}

/// Write `doc/index.html`: the language overview and navigation hub.
fn write_index(topics: &[Topic]) -> std::io::Result<()> {
    // Build the topic card grid.
    let cards: String = topics.iter().fold(String::new(), |mut output, topic| {
        if !topic.filename.starts_with("00-") {
            let _ = writeln!(
                output,
                "    <a class=\"card\" href=\"{}.html\"><h2>{}</h2><p>{}</p></a>",
                topic.filename, topic.name, topic.title
            );
        }
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
    <h2 class=\"topics-heading\">Topics</h2>\n\
    <div class=\"grid\">\n\
{cards}\
    </div>\n\
  </section>\n\
</body>\n\
</html>\n"
    );
    std::fs::write("doc/index.html", html)
}

/// Escape `&`, `<`, and `>` so that `s` is safe to embed in HTML text content.
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

/// Split a `.lav` source file into alternating prose (comments) and code
/// sections, stripping the copyright header at the top.
fn parse_sections(source: &str) -> Vec<DocSection> {
    let mut sections: Vec<DocSection> = Vec::new();
    let mut prose: Vec<String> = Vec::new();
    let mut code: Vec<String> = Vec::new();
    // Skip lines that are part of the copyright/SPDX block at the top.
    let mut in_header = true;

    for line in source.lines() {
        let trimmed = line.trim();
        if in_header && skip_header(trimmed) {
            continue;
        }
        in_header = false;

        if trimmed.starts_with("//") {
            // Flush any pending code block before starting prose.
            if !code.is_empty() {
                sections.push(DocSection::Code(std::mem::take(&mut code)));
            }
            let text = trimmed.strip_prefix("//").unwrap_or("").trim().to_string();
            prose.push(text);
        } else if trimmed.is_empty() {
            // Empty line ends both kinds of blocks.
            if !prose.is_empty() {
                sections.push(DocSection::Prose(std::mem::take(&mut prose)));
            }
            if !code.is_empty() {
                sections.push(DocSection::Code(std::mem::take(&mut code)));
            }
        } else {
            // Flush any pending prose before accumulating code.
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

/// Scan a quoted literal (string `"` or char `'`) starting at `i`.
/// Returns the index after the closing delimiter, or `n` if unclosed.
fn scan_quoted(chars: &[char], i: usize, n: usize, delim: char) -> usize {
    let mut j = i + 1;
    while j < n && chars[j] != delim {
        j += 1;
    }
    if j < n { j + 1 } else { j }
}

/// Scan a numeric literal starting at `i`. Returns the index after the last digit/suffix.
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
    // Optional suffix: l (long) or f (single).
    if j < n && (chars[j] == 'l' || chars[j] == 'f') {
        j += 1;
    }
    j
}

/// Return the CSS highlight class for an identifier word.
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

/// Apply HTML syntax highlighting to a block of source code.
/// Returns HTML with `<span class="…">` wrappers for each token class.
fn highlight_loft(code: &str) -> String {
    let mut out = String::with_capacity(code.len() * 2);

    for line in code.lines() {
        let chars: Vec<char> = line.chars().collect();
        let no_chars = chars.len();
        let mut i = 0;

        while i < no_chars {
            // Line comment: the rest of the line is gray.
            if i + 1 < no_chars && chars[i] == '/' && chars[i + 1] == '/' {
                let rest: String = chars[i..].iter().collect();
                out.push_str("<span class=\"cm\">");
                out.push_str(&html_esc(&rest));
                out.push_str("</span>");
                i = no_chars;
                continue;
            }

            // String literal.
            if chars[i] == '"' {
                let j = scan_quoted(&chars, i, no_chars, '"');
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"st\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Character literal.
            if chars[i] == '\'' {
                let j = scan_quoted(&chars, i, no_chars, '\'');
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"ch\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Numeric literal.
            if chars[i].is_ascii_digit() {
                let j = scan_number(&chars, i, no_chars);
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"nm\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Identifier, keyword, type, or builtin.
            if chars[i].is_alphabetic() || chars[i] == '_' {
                let mut j = i;
                while j < no_chars && (chars[j].is_alphanumeric() || chars[j] == '_') {
                    j += 1;
                }
                let word: String = chars[i..j].iter().collect();
                // Peek past whitespace for '(' to detect function calls.
                let mut k = j;
                while k < no_chars && chars[k] == ' ' {
                    k += 1;
                }
                let cls = word_class(&word, k < no_chars && chars[k] == '(');
                if cls.is_empty() {
                    out.push_str(&html_esc(&word));
                } else {
                    out.push_str("<span class=\"");
                    out.push_str(cls);
                    out.push_str("\">");
                    out.push_str(&html_esc(&word));
                    out.push_str("</span>");
                }
                i = j;
                continue;
            }

            // Everything else (operators, punctuation).
            out.push_str(&html_esc(&chars[i].to_string()));
            i += 1;
        }
        out.push('\n');
    }

    // Trim the trailing newline added after the last line.
    if out.ends_with('\n') {
        out.pop();
    }
    out
}

// ─── HTML page renderer ───────────────────────────────────────────────────────

/// Build the complete HTML for one documentation page.
///
/// `source` is the raw text of a `.lav` suite file.  It is split by
/// `parse_sections` into alternating prose (comment) and code blocks.  Code
/// blocks are syntax-highlighted by `highlight_loft` and wrapped in `<pre>`;
/// prose blocks become `<p>` paragraphs.
///
/// `nav` is the ordered list of all pages (stem plus short title).  The entry at
/// `idx` is rendered as the bolded current page; all others become links.
/// A fixed link to the 'vs-Rust' comparison page is prepended to the navigation bar.
fn render_doc_page(source: &str, topics: &[Topic], idx: usize) -> String {
    // Navigation bar: all pages listed, current one bolded.
    let nav_html: String = std::iter::once("<a href=\"00-vs-rust.html\">vs Rust</a>".to_string())
        .chain(
            topics
                .iter()
                .enumerate()
                .filter(|(i, _)| *i > 0)
                .map(|(i, topic)| {
                    if i == idx {
                        format!("<span class=\"cur\">{}</span>", topic.name)
                    } else {
                        format!("<a href=\"{}.html\">{}</a>", topic.filename, topic.name)
                    }
                }),
        )
        .collect::<Vec<_>>()
        .join(" · ");

    // Build the article body from alternating prose/code sections.
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
                let highlighted = highlight_loft(&lines.join("\n"));
                body.push_str("<pre><code>");
                body.push_str(&highlighted);
                body.push_str("</code></pre>\n");
            }
        }
    }

    format!(
        "<!DOCTYPE html>\n\
<html lang=\"en\">\n\
<head>\n\
  <meta charset=\"utf-8\">\n\
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
  <title>Loft - {name}</title>\n\
  <link rel=\"stylesheet\" href=\"style.css\">\n\
</head>\n\
<body>\n\
  <nav>{nav_html}</nav>\n\
  <h1>{name}</h1>\n\
  <article>\n{body}\n  </article>\n\
</body>\n\
</html>\n",
        name = topics[idx].name
    )
}
