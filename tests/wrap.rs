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

/// Run every `.lav` file in `tests/suite/` in alphabetical order.
/// Regenerates all HTML documentation in `doc/` before executing the tests,
/// so the docs are always in sync with the suite source files.
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

/// Quick iteration test: run only the final suite file (`16-parser.lav`) without
/// regenerating documentation.  Use this during active development on the parser
/// to get a fast feedback cycle.
#[test]
fn last() -> std::io::Result<()> {
    run_test(PathBuf::from("tests/suite/16-parser.lav"), false)
}

/// Parse, type-check, compile, and execute one `.lav` test file.
///
/// The default library in `default/` is loaded first, then `entry` is parsed on
/// top of it.  Any parse or type errors are printed and immediately fail the
/// test.  On success the bytecode is generated and `main` is called.
///
/// In debug builds a human-readable bytecode dump is written to
/// `tests/code/<filename>.txt` before execution.  When `debug` is true the
/// interpreter also emits an execution trace to that file.
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

/// Write a debug snapshot of a compiled test to `tests/code/<filename>.txt`.
///
/// Writes every type definition introduced by the test file (i.e. types beyond
/// those already present in the default library), followed by the full bytecode
/// listing produced by `show_code`.  Returns the open file so the caller can
/// append an execution trace if needed.
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
    std::fs::write("doc/style.css", STYLE_CSS)?;
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
  <link rel=\"stylesheet\" href=\"style.css\">\n\
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

/// Apply HTML syntax highlighting to a block of lav source code.
/// Returns HTML with `<span class="…">` wrappers for each token class.
fn highlight_lav(code: &str) -> String {
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

            // String literal.
            if chars[i] == '"' {
                let j = scan_quoted(&chars, i, n, '"');
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"st\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Character literal.
            if chars[i] == '\'' {
                let j = scan_quoted(&chars, i, n, '\'');
                let s: String = chars[i..j].iter().collect();
                out.push_str("<span class=\"ch\">");
                out.push_str(&html_esc(&s));
                out.push_str("</span>");
                i = j;
                continue;
            }

            // Numeric literal.
            if chars[i].is_ascii_digit() {
                let j = scan_number(&chars, i, n);
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
                // Peek past whitespace for '(' to detect function calls.
                let mut k = j;
                while k < n && chars[k] == ' ' {
                    k += 1;
                }
                let cls = word_class(&word, k < n && chars[k] == '(');
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
/// blocks are syntax-highlighted by `highlight_lav` and wrapped in `<pre>`;
/// prose blocks become `<p>` paragraphs.
///
/// `nav` is the ordered list of all pages (stem + short title).  The entry at
/// `idx` is rendered as the bolded current page; all others become links.
/// A fixed link to the vs-Rust comparison page is prepended to the nav bar.
fn render_doc_page(source: &str, nav: &[(String, String)], idx: usize) -> String {
    let stem = &nav[idx].0;
    let full_title = doc_full_title(stem);

    // Navigation bar: all pages listed, current one bolded.
    let nav_html: String = std::iter::once("<a href=\"00-vs-rust.html\">vs Rust</a>".to_string())
        .chain(nav.iter().enumerate().map(|(i, (s, t))| {
            if i == idx {
                format!("<span class=\"cur\">{t}</span>")
            } else {
                format!("<a href=\"{s}.html\">{t}</a>")
            }
        }))
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
  <link rel=\"stylesheet\" href=\"style.css\">\n\
</head>\n\
<body>\n\
  <nav>{nav_html}</nav>\n\
  <h1>{full_title}</h1>\n\
  <article>\n{body}\n  </article>\n\
</body>\n\
</html>\n"
    )
}

/// Shared stylesheet written to `doc/style.css` on every doc-generation run.
/// All generated HTML pages reference it via `<link rel="stylesheet" href="style.css">`.
/// Edit this constant to change the look of all documentation pages.
const STYLE_CSS: &str = r#"/* Copyright (c) 2025 Jurjen Stellingwerff                        */
/* SPDX-License-Identifier: LGPL-3.0-or-later                     */
/* Shared stylesheet for all Lav documentation pages.              */
/* Generated by tests/wrap.rs — edit the STYLE_CSS constant there. */

:root {
  --bg: #fff;
  --text: #1a1a2e;
  --code-bg: #f8f9fa;
  --border: #e5e7eb;
  --accent: #2563eb;
  --nav-bg: #f3f4f6;
  --kw: #d1242f;
  --ty: #8250df;
  --st: #0a3069;
  --cm: #6a737d;
  --nm: #0550ae;
  --bi: #953800;
  --en: #1a7f37;
  --dim: #6b7280;
}

*, *::before, *::after { box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
  font-size: 16px;
  line-height: 1.7;
  color: var(--text);
  background: var(--bg);
  max-width: 900px;
  margin: 0 auto;
  padding: 24px 32px;
}

nav {
  background: var(--nav-bg);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 10px 16px;
  margin-bottom: 32px;
  line-height: 2.2;
  font-size: 0.9em;
}
nav a { color: var(--accent); text-decoration: none; margin: 0 3px; }
nav a:hover { text-decoration: underline; }
.cur { font-weight: 700; color: var(--text); margin: 0 3px; }

h1 {
  font-size: 1.75em;
  margin: 0 0 24px;
  padding-bottom: 10px;
  border-bottom: 2px solid var(--border);
}
h2 { font-size: 1.1em; font-weight: 700; margin: 2em 0 0.4em; }

p { margin: 0.4em 0 1em; color: #374151; }

pre {
  background: var(--code-bg);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 14px 16px;
  overflow-x: auto;
  margin: 0 0 18px;
  font-size: 0.875em;
  line-height: 1.55;
}

code {
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
}

/* Syntax highlight classes */
.cm { color: var(--cm); font-style: italic; }
.kw { color: var(--kw); font-weight: 600; }
.ty { color: var(--ty); }
.st, .ch { color: var(--st); }
.nm { color: var(--nm); }
.bi { color: var(--bi); }
.en { color: var(--en); }
.fn-call { color: #005cc5; }

/* stdlib links inside code blocks: inherit span colour, subtle underline */
pre a { color: inherit; text-decoration: underline dotted; }
pre a:hover { text-decoration: underline; }

/* ── Index page ─────────────────────────────────────────────────────────── */

header {
  text-align: center;
  padding: 48px 0 32px;
  border-bottom: 2px solid var(--border);
  margin-bottom: 32px;
}
header h1 { font-size: 3em; margin: 0 0 8px; letter-spacing: -1px; }
.tagline { font-size: 1.1em; color: var(--dim); margin: 0; }

.intro { margin-bottom: 40px; }
.intro ul { padding-left: 1.4em; }
.intro li { margin: 6px 0; color: #374151; }

.topics-heading {
  font-size: 1.3em;
  margin: 0 0 16px;
  color: var(--dim);
  text-transform: uppercase;
  letter-spacing: 1px;
  font-weight: 600;
}

.grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(240px, 1fr));
  gap: 14px;
}

.card {
  display: block;
  padding: 16px 18px;
  background: var(--code-bg);
  border: 1px solid var(--border);
  border-radius: 8px;
  text-decoration: none;
  color: inherit;
  transition: border-color .15s, box-shadow .15s;
}
.card:hover { border-color: var(--accent); box-shadow: 0 2px 8px rgba(37,99,235,.12); }
.card h2 { font-size: 1em; margin: 0 0 6px; color: var(--accent); }
.card p { font-size: .85em; margin: 0; color: #4b5563; line-height: 1.5; }

/* ── vs-Rust comparison page ────────────────────────────────────────────── */

.compare {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 10px;
  margin: 0.6em 0 0.8em;
}

pre.rust-pre { background: #fff8f0; border-color: #fcd9ad; }

.lang-label {
  font-size: 0.7em;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.1em;
  margin-bottom: 4px;
}
.lav-label  { color: #2563eb; }
.rust-label { color: #b45309; }

.verdict {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 10px;
  margin: 0.4em 0 1em;
}

.up, .down {
  margin: 0;
  padding: 0.5em 0.75em;
  border-radius: 5px;
  font-size: 0.92em;
  line-height: 1.5;
}
.up   { background: #f0fdf4; border: 1px solid #bbf7d0; color: #166534; }
.down { background: #fff1f2; border: 1px solid #fecdd3; color: #9f1239; }
.up   strong, .down strong {
  display: block;
  font-size: 0.8em;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  margin-bottom: 0.2em;
}

/* vs-rust section headers */
section-header, h2.diff {
  padding: 0.3em 0.7em;
  background: var(--nav-bg);
  border-left: 4px solid var(--accent);
  border-radius: 0 4px 4px 0;
}
"#;
