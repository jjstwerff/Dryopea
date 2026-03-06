// Copyright (c) 2022-2025 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

extern crate dryopea;

#[cfg(debug_assertions)]
use dryopea::data::Data;
use dryopea::interpreter::byte_code;
#[cfg(debug_assertions)]
use dryopea::interpreter::show_code;
use dryopea::parser::Parser;
use dryopea::scopes;
use dryopea::state::State;
#[cfg(debug_assertions)]
use std::fs::File;
#[cfg(debug_assertions)]
use std::io::{Error, Write};
use std::path::PathBuf;

#[test]
fn dir() -> std::io::Result<()> {
    let mut files: Vec<PathBuf> = std::fs::read_dir("tests/suite")?
        .filter_map(|f| f.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|e| e.eq_ignore_ascii_case("lav")))
        .collect();
    files.sort();
    generate_docs(&files)?;
    for entry in files {
        run_test(entry, false)?;
    }
    Ok(())
}

#[test]
fn last() -> std::io::Result<()> {
    run_test(PathBuf::from("tests/suite/16-parser.lav"), false)
}

fn run_test(entry: PathBuf, debug: bool) -> std::io::Result<()> {
    println!("run {entry:?}");
    let mut p = Parser::new();
    p.parse_dir("default", true, debug)?;
    #[cfg(debug_assertions)]
    let types = p.database.types.len();
    let path = entry.to_string_lossy().to_string();
    p.parse(&path, false);
    for l in p.diagnostics.lines() {
        println!("{l}");
    }
    if !p.diagnostics.is_empty() {
        return Err(Error::from(std::io::ErrorKind::InvalidData));
    }
    scopes::check(&mut p.data);
    let mut state = State::new(p.database);
    byte_code(&mut state, &mut p.data);
    #[cfg(debug_assertions)]
    let mut w = dump_results(entry, &mut p.data, types, &mut state)?;
    if debug {
        #[cfg(debug_assertions)]
        state.execute_log(&mut w, "main", &p.data)?;
        #[cfg(not(debug_assertions))]
        state.execute("main", &p.data);
    } else {
        state.execute("main", &p.data);
    }
    Ok(())
}

#[cfg(debug_assertions)]
fn dump_results(
    entry: PathBuf,
    data: &mut Data,
    types: usize,
    state: &mut State,
) -> Result<File, Error> {
    let filename = entry.file_name().unwrap_or_default().to_string_lossy();
    let mut w = File::create(format!("tests/code/{filename}.txt"))?;
    for tp in types..state.database.types.len() {
        writeln!(
            &mut w,
            "Type {tp}:{}",
            state.database.show_type(tp as u16, true)
        )?;
    }
    show_code(&mut w, state, data)?;
    Ok(w)
}

// ─── Documentation generator ─────────────────────────────────────────────────

/// One-line description for each numbered suite file, matched by stem prefix.
/// Order mirrors the file numbering; unrecognised files get an empty description.
const TOPIC_DESCRIPTIONS: &[(&str, &str)] = &[
    (
        "01",
        "Core syntax: if/else expressions, for loops, break/continue, \
         and named breaks for nested loops.",
    ),
    (
        "02",
        "UTF-8 strings: byte-based indexing, slices, concatenation, \
         formatting, and built-in search functions.",
    ),
    (
        "03",
        "32-bit integers: arithmetic, conversions to/from text and float, \
         bitwise operations, and null from division by zero.",
    ),
    (
        "04",
        "Boolean logic: &&/|| and and/or, bitwise &/|/&lt;&lt;/&gt;&gt;, \
         and null as the absent/falsy value.",
    ),
    (
        "05",
        "64-bit floating-point: arithmetic, the ^ exponentiation operator, \
         math functions (sin, cos, log, round, ceil, floor), and format specifiers.",
    ),
    (
        "06",
        "Functions: parameter defaults, in-out references with &amp;, \
         early return, and implicit return of the last expression.",
    ),
    (
        "07",
        "Dynamic arrays: literals, slices, filtered removal with #remove, \
         reverse iteration, and for-loops embedded in format strings.",
    ),
    (
        "08",
        "Record types: field constraints (limit, not null), computed fields \
         using $, methods, JSON (:j) and pretty-print (:#) formatting.",
    ),
    (
        "09",
        "Enums: plain value enums with ordering and text conversion; \
         struct enums with field access and polymorphic dispatch.",
    ),
    (
        "10",
        "sorted&lt;T[key]&gt;: O(log n) key lookup and in-order iteration \
         in a sorted binary tree; ascending and descending sort keys.",
    ),
    (
        "11",
        "index&lt;T[key]&gt;: balanced red-black tree with strict sorted order, \
         multi-key lookup, and range iteration.",
    ),
    (
        "12",
        "hash&lt;T[key]&gt;: O(1) average-time key lookup; \
         combining a hash with a vector for ordered traversal and fast access.",
    ),
    (
        "13",
        "File system access: creating File handles, listing directory contents, \
         reading file text, and splitting into lines.",
    ),
    (
        "14",
        "Image loading and pixel-level manipulation using the built-in Image type.",
    ),
    (
        "15",
        "Built-in lexer library: tokenising source text, handling multi-character \
         operators, comments, string literals, and formatting expressions.",
    ),
    (
        "16",
        "Built-in parser library: parsing lav source code programmatically \
         from within a lav program.",
    ),
];

/// Look up the description for a file whose stem starts with `prefix` (e.g. "06").
fn topic_description(stem: &str) -> &'static str {
    let prefix = &stem[..stem.find('-').unwrap_or(stem.len()).min(2)];
    TOPIC_DESCRIPTIONS
        .iter()
        .find(|(p, _)| *p == prefix)
        .map_or("", |(_, d)| d)
}

/// Render an HTML documentation page for every `.lav` suite file and write it
/// to `doc/<stem>.html`, then write `doc/index.html`.
/// Called unconditionally from `dir()` so docs are always up-to-date.
fn generate_docs(entries: &[PathBuf]) -> std::io::Result<()> {
    let nav: Vec<(String, String)> = entries
        .iter()
        .map(|e| {
            let stem = e
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned();
            let title = doc_short_title(&stem);
            (stem, title)
        })
        .collect();
    for (idx, entry) in entries.iter().enumerate() {
        let stem = entry
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();
        if let Ok(source) = std::fs::read_to_string(entry) {
            let html = render_doc_page(&source, &nav, idx);
            std::fs::write(format!("doc/{stem}.html"), html)?;
        }
    }
    write_index(&nav)?;
    Ok(())
}

/// Write `doc/index.html`: the language overview and navigation hub.
fn write_index(nav: &[(String, String)]) -> std::io::Result<()> {
    // Build the topic card grid.
    let cards: String = nav
        .iter()
        .map(|(stem, title)| {
            let desc = topic_description(stem);
            format!(
                "    <a class=\"card\" href=\"{stem}.html\">\
<h2>{title}</h2><p>{desc}</p></a>\n"
            )
        })
        .collect();

    let html = format!(
        "<!DOCTYPE html>\n\
<html lang=\"en\">\n\
<head>\n\
  <meta charset=\"utf-8\">\n\
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
  <title>Lav Language</title>\n\
  <style>\n\
{DOC_CSS}\n\
{INDEX_EXTRA_CSS}\n\
  </style>\n\
</head>\n\
<body>\n\
  <header>\n\
    <h1>Lav</h1>\n\
    <p class=\"tagline\">A statically-typed scripting language for the Dryopea engine</p>\n\
  </header>\n\
  <section class=\"intro\">\n\
    <p>Lav is an imperative language inspired by Rust but designed to be concise and \
easy to embed. Every example in this documentation is a live, executable test — \
the pages below are generated directly from the <code>tests/suite/</code> source files.</p>\n\
    <p>Key characteristics:</p>\n\
    <ul>\n\
      <li>Variables are declared on first assignment; types are inferred.</li>\n\
      <li>Functions use implicit return: the last expression in a block is the result.</li>\n\
      <li>Null is a typed absence value; any field or variable can be null unless marked \
<code>not null</code>.</li>\n\
      <li>Strings are UTF-8; <code>len()</code> counts bytes and indexing is byte-based.</li>\n\
      <li>Collections — <code>vector</code>, <code>sorted</code>, <code>index</code>, \
<code>hash</code> — share records and can be combined in a single struct.</li>\n\
      <li><code>^</code> is exponentiation (not bitwise XOR); bitwise operators are \
<code>&amp;</code>, <code>|</code>, <code>&lt;&lt;</code>, <code>&gt;&gt;</code>.</li>\n\
      <li>Polymorphic dispatch is supported for struct-enum variants.</li>\n\
    </ul>\n\
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

/// "02-text" → "Text"
fn doc_short_title(stem: &str) -> String {
    let name = stem.find('-').map_or(stem, |p| &stem[p + 1..]);
    let mut it = name.chars();
    it.next()
        .map_or_else(String::new, |c| c.to_uppercase().to_string() + it.as_str())
}

/// "02-text" → "02 — Text"
fn doc_full_title(stem: &str) -> String {
    if let Some(p) = stem.find('-') {
        format!("{} — {}", &stem[..p], doc_short_title(stem))
    } else {
        stem.to_string()
    }
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

        if in_header {
            if trimmed.starts_with("// Copyright")
                || trimmed.starts_with("// SPDX")
                || trimmed.is_empty()
            {
                continue;
            }
            in_header = false;
        }

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

/// Apply HTML syntax highlighting to a block of lav source code.
/// Returns HTML with `<span class="…">` wrappers for each token class.
fn highlight_lav(code: &str) -> String {
    const KW: &[&str] = &[
        "fn", "if", "else", "for", "in", "return", "break", "continue", "struct", "enum", "pub",
        "use", "type", "as", "not", "null", "true", "false", "and", "or", "limit", "default",
        "virtual",
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
        "assert", "panic", "len", "round", "ceil", "floor", "abs", "sin", "cos", "log", "rev",
        "file", "min", "max", "sqrt", "typeof", "typedef",
    ];

    let mut out = String::with_capacity(code.len() * 2);

    for line in code.lines() {
        let chars: Vec<char> = line.chars().collect();
        let n = chars.len();
        let mut i = 0;

        while i < n {
            // Line comment: rest of line is grey.
            if i + 1 < n && chars[i] == '/' && chars[i + 1] == '/' {
                let rest: String = chars[i..].iter().collect();
                out.push_str("<span class=\"cm\">");
                out.push_str(&html_esc(&rest));
                out.push_str("</span>");
                i = n;
                continue;
            }

            // String literal: find the closing '"'.
            if chars[i] == '"' {
                let mut j = i + 1;
                while j < n && chars[j] != '"' {
                    j += 1;
                }
                if j < n {
                    j += 1;
                }
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"st\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Character literal: find the closing '\''.
            if chars[i] == '\'' {
                let mut j = i + 1;
                while j < n && chars[j] != '\'' {
                    j += 1;
                }
                if j < n {
                    j += 1;
                }
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"ch\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Numeric literal.
            if chars[i].is_ascii_digit() {
                let mut j = i;
                // Hex / binary / octal prefix.
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
                            while j < n
                                && (chars[j].is_ascii_digit() || chars[j] == '.' || chars[j] == '_')
                            {
                                j += 1;
                            }
                        }
                    }
                } else {
                    while j < n && (chars[j].is_ascii_digit() || chars[j] == '.' || chars[j] == '_')
                    {
                        j += 1;
                    }
                }
                // Optional suffix: l (long) or f (single).
                if j < n && (chars[j] == 'l' || chars[j] == 'f') {
                    j += 1;
                }
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
                while j < n && (chars[j].is_alphanumeric() || chars[j] == '_') {
                    j += 1;
                }
                let word: String = chars[i..j].iter().collect();

                // Peek past whitespace for '(' to detect calls.
                let mut k = j;
                while k < n && chars[k] == ' ' {
                    k += 1;
                }
                let is_call = k < n && chars[k] == '(';
                let first_upper = word.starts_with(|c: char| c.is_uppercase());

                let cls = if KW.contains(&word.as_str()) {
                    "kw"
                } else if TY.contains(&word.as_str()) {
                    "ty"
                } else if BI.contains(&word.as_str()) {
                    "bi"
                } else if first_upper {
                    "en"
                } else if is_call {
                    "fn-call"
                } else {
                    ""
                };

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

fn render_doc_page(source: &str, nav: &[(String, String)], idx: usize) -> String {
    let stem = &nav[idx].0;
    let full_title = doc_full_title(stem);

    // Navigation bar: all pages listed, current one bolded.
    let nav_html: String = nav
        .iter()
        .enumerate()
        .map(|(i, (s, t))| {
            if i == idx {
                format!("<span class=\"cur\">{t}</span>")
            } else {
                format!("<a href=\"{s}.html\">{t}</a>")
            }
        })
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
                let highlighted = highlight_lav(&lines.join("\n"));
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
  <title>Lav — {full_title}</title>\n\
  <style>\n{DOC_CSS}\n  </style>\n\
</head>\n\
<body>\n\
  <nav>{nav_html}</nav>\n\
  <h1>{full_title}</h1>\n\
  <article>\n{body}\n  </article>\n\
</body>\n\
</html>\n"
    )
}

const INDEX_EXTRA_CSS: &str = "\
header { text-align: center; padding: 48px 0 32px; border-bottom: 2px solid var(--border); margin-bottom: 32px; }\n\
header h1 { font-size: 3em; margin: 0 0 8px; letter-spacing: -1px; }\n\
.tagline { font-size: 1.1em; color: var(--dim); margin: 0; }\n\
.intro { margin-bottom: 40px; }\n\
.intro ul { padding-left: 1.4em; }\n\
.intro li { margin: 6px 0; color: #374151; }\n\
.topics-heading { font-size: 1.3em; margin: 0 0 16px; color: var(--dim); text-transform: uppercase; letter-spacing: 1px; font-weight: 600; }\n\
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: 14px; }\n\
.card { display: block; padding: 16px 18px; background: var(--code-bg); border: 1px solid var(--border); border-radius: 8px; text-decoration: none; color: inherit; transition: border-color .15s, box-shadow .15s; }\n\
.card:hover { border-color: var(--accent); box-shadow: 0 2px 8px rgba(37,99,235,.12); }\n\
.card h2 { font-size: 1em; margin: 0 0 6px; color: var(--accent); }\n\
.card p { font-size: .85em; margin: 0; color: #4b5563; line-height: 1.5; }";

const DOC_CSS: &str = "\
:root {\
  --bg:#fff; --text:#1a1a2e; --code-bg:#f8f9fa;\
  --border:#e5e7eb; --accent:#2563eb; --nav-bg:#f3f4f6;\
  --kw:#d1242f; --ty:#8250df; --st:#0a3069; --cm:#6a737d;\
  --nm:#0550ae; --bi:#953800; --en:#1a7f37;\
}\n\
*, *::before, *::after { box-sizing: border-box; }\n\
body {\
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;\
  font-size: 16px; line-height: 1.7; color: var(--text); background: var(--bg);\
  max-width: 860px; margin: 0 auto; padding: 24px 32px;\
}\n\
nav {\
  background: var(--nav-bg); border: 1px solid var(--border); border-radius: 8px;\
  padding: 10px 16px; margin-bottom: 32px; line-height: 2.2; font-size: 0.9em;\
}\n\
nav a { color: var(--accent); text-decoration: none; margin: 0 3px; }\n\
nav a:hover { text-decoration: underline; }\n\
.cur { font-weight: 700; color: var(--text); margin: 0 3px; }\n\
h1 { font-size: 1.75em; margin: 0 0 24px; padding-bottom: 10px; border-bottom: 2px solid var(--border); }\n\
p { margin: 0.4em 0 1em; color: #374151; }\n\
pre {\
  background: var(--code-bg); border: 1px solid var(--border); border-radius: 6px;\
  padding: 14px 16px; overflow-x: auto; margin: 0 0 18px;\
  font-size: 0.875em; line-height: 1.55;\
}\n\
code { font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace; }\n\
.cm { color: var(--cm); font-style: italic; }\n\
.kw { color: var(--kw); font-weight: 600; }\n\
.ty { color: var(--ty); }\n\
.st, .ch { color: var(--st); }\n\
.nm { color: var(--nm); }\n\
.bi { color: var(--bi); }\n\
.en { color: var(--en); }\n\
.fn-call { color: #005cc5; }";
