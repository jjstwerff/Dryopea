// Copyright (c) 2022-2023 Jurjen Stellingwerff
// SPDX-License-Identifier: LGPL-3.0-or-later

//! Change a text into symbols to use in the parser.
//! It is possible to link to the current position in the lexer (link) and return to it (revert)
//! when the parser has to try a certain path and might dismiss this later.

use crate::diagnostics::*;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::io::Result as IoResult;
use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

#[derive(Debug, Clone, PartialEq)]
pub enum Mode {
    /// Expect code with spaces, line ends and remarks removed.
    Code,
    /// Expect formatting expressions, when encountering a closing bracket continue with a string.
    Formatting,
}

/// An item parsed by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum LexItem {
    /// This routine cannot directly parse negative number, because - is reported as a token.
    /// Second token is if the number started with a 0. Only needed for string formatting.
    Integer(u32, bool),
    Long(u64),
    Float(f64),
    Single(f32),
    /// Can be both a keyword and a one or two position token.
    Token(String),
    /// A still unknown identifier.
    Identifier(String),
    /// A constant string: was presented as "content" with possibly escaped tokens inside.
    CString(String),
    /// The end of the content is reached.
    None,
}

#[derive(Clone, PartialEq)]
pub struct Position {
    /// The file name where this construct is found.
    pub file: String,
    /// The line where this result was found.
    pub line: u32,
    /// The position on the line where this result was found.
    pub pos: u32,
}

impl Position {
    fn format(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(&format!("{} line {}:{}", self.file, self.line, self.pos))
    }
}

impl Debug for Position {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.format(fmt)
    }
}

impl Display for Position {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.format(fmt)
    }
}

/// The lexer can be iterated to gain a string of results.
#[derive(Debug, Clone, PartialEq)]
pub struct LexResult {
    pub has: LexItem,
    pub position: Position,
}

impl LexResult {
    fn new(it: LexItem, position: Position) -> LexResult {
        LexResult { has: it, position }
    }
}

/// A lexer that can remember a state via a link and then optionally return to that state.
///
/// It defaults to reading all found data into Text elements but has a list of TOKENS and
/// KEYWORDS that are parsed when a line starts with a token.
pub struct Lexer {
    lines: Box<dyn Iterator<Item = IoResult<String>>>,
    iter: Peekable<IntoIter<char>>,
    peek: LexResult,
    /// Keep the scanned items in memory when a Link is created to return when reverted to this link.
    memory: Vec<LexResult>,
    /// Keep track of the number of currently in use links
    links: Rc<RefCell<u32>>,
    /// Keep track of where we are in the current memory structure
    link: usize,
    position: Position,
    tokens: HashSet<String>,
    keywords: HashSet<String>,
    /// Should we expect code with whitespaces here?
    mode: Mode,
    diagnostics: Diagnostics,
}

impl Debug for Lexer {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.write_str(&format!("{:?}", self.position))
    }
}

static LINE: String = String::new();

static TOKENS: &[&str] = &[
    ":", "::", ".", "..", ",", "{", "}", "(", ")", "[", "]", ";", "!", "!=", "+", "+=", "-", "-=",
    "*", "*=", "/", "/=", "%", "%=", "=", "==", "<", "<=", ">", ">=", "&", "&&", "|", "||", "->",
    "=>", "^", "$", "//", "#",
];

static KEYWORDS: &[&str] = &[
    "as", "if", "in", "else", "for", "continue", "break", "return", "true", "false", "null",
    "struct", "fn", "type", "enum", "pub",
];

#[derive(Debug)]
pub struct Link {
    links: Rc<RefCell<u32>>,
    pos: usize,
}

impl Drop for Link {
    fn drop(&mut self) {
        *self.links.borrow_mut() -= 1;
    }
}

fn hex_parse(val: &str) -> Option<u64> {
    let mut res: u64 = 0;
    for ch in val.chars() {
        if ch.is_ascii_digit() {
            res = res * 16 + ch as u64 - '0' as u64;
        } else if ('a'..='f').contains(&ch) {
            res = res * 16 + 10 + ch as u64 - 'a' as u64;
        } else {
            return None;
        }
    }
    Some(res)
}

fn bin_parse(val: &str) -> Option<u64> {
    let mut res: u64 = 0;
    for ch in val.chars() {
        if ('0'..='1').contains(&ch) {
            res = res * 2 + ch as u64 - '0' as u64;
        } else {
            return None;
        }
    }
    Some(res)
}

fn oct_parse(val: &str) -> Option<u64> {
    let mut res: u64 = 0;
    for ch in val.chars() {
        if ('0'..='7').contains(&ch) {
            res = res * 8 + ch as u64 - '0' as u64;
        } else {
            return None;
        }
    }
    Some(res)
}

impl Default for Lexer {
    fn default() -> Self {
        Lexer {
            lines: Box::new(Vec::new().into_iter()),
            peek: LexResult {
                has: LexItem::None,
                position: Position {
                    file: "".to_string(),
                    line: 0,
                    pos: 0,
                },
            },
            position: Position {
                file: "".to_string(),
                line: 0,
                pos: 0,
            },
            memory: Vec::new(),
            link: 0,
            links: Rc::new(RefCell::new(0)),
            iter: LINE.chars().collect::<Vec<_>>().into_iter().peekable(),
            tokens: HashSet::new(),
            keywords: HashSet::new(),
            mode: Mode::Code,
            diagnostics: Diagnostics::new(),
        }
    }
}

impl Lexer {
    fn new(lines: impl Iterator<Item = IoResult<String>> + 'static, filename: &str) -> Lexer {
        let mut result = Lexer {
            lines: Box::new(lines),
            peek: LexResult {
                has: LexItem::None,
                position: Position {
                    file: filename.to_string(),
                    line: 0,
                    pos: 0,
                },
            },
            position: Position {
                file: filename.to_string(),
                line: 0,
                pos: 0,
            },
            memory: Vec::new(),
            link: 0,
            links: Rc::new(RefCell::new(0)),
            iter: LINE.chars().collect::<Vec<_>>().into_iter().peekable(),
            tokens: HashSet::new(),
            keywords: HashSet::new(),
            mode: Mode::Code,
            diagnostics: Diagnostics::new(),
        };
        for s in TOKENS {
            result.tokens.insert(String::from(*s));
        }
        for s in KEYWORDS {
            result.keywords.insert(String::from(*s));
        }
        result
    }

    fn next(&mut self) -> Option<LexResult> {
        if self.link < self.memory.len() {
            let n = self.memory[self.link].clone();
            self.link += 1;
            return Some(n);
        }
        if self.mode != Mode::Formatting {
            loop {
                if let Some(&c) = self.iter.peek() {
                    if c != ' ' && c != '\t' {
                        break;
                    }
                    self.next_char();
                } else if let Some(Ok(ln)) = self.lines.next() {
                    self.iter = ln.chars().collect::<Vec<_>>().into_iter().peekable();
                    self.position.line += 1;
                    self.position.pos = 1;
                } else {
                    break;
                }
            }
        }
        let pos = self.position.clone();
        if let Some(&c) = self.iter.peek() {
            Some(match c {
                '0'..='9' => self.number(),
                '"' => {
                    self.next_char();
                    self.string()
                }
                ' ' | '\t' => {
                    self.next_char();
                    LexResult::new(LexItem::Token(" ".to_string()), pos)
                }
                _ => {
                    let single = String::from(c);
                    if self.tokens.contains(&single) {
                        self.next_char();
                        if let Some(&d) = self.iter.peek() {
                            let double = format!("{}{}", c, d);
                            if self.tokens.contains(&double) {
                                self.next_char();
                                LexResult::new(LexItem::Token(double), pos)
                            } else if self.mode == Mode::Formatting && single == "}" {
                                self.string()
                            } else {
                                LexResult::new(LexItem::Token(single), pos)
                            }
                        } else {
                            LexResult::new(LexItem::Token(single), pos)
                        }
                    } else {
                        let ident = self.get_identifier();
                        if self.keywords.contains(&ident) {
                            LexResult::new(LexItem::Token(ident), pos)
                        } else {
                            LexResult::new(LexItem::Identifier(ident), pos)
                        }
                    }
                }
            })
        } else if let Some(Ok(ln)) = self.lines.next() {
            self.iter = ln.chars().collect::<Vec<_>>().into_iter().peekable();
            self.position.line += 1;
            self.position.pos = 1;
            Some(LexResult::new(LexItem::None, self.position.clone()))
        } else {
            None
        }
    }

    pub fn pos(&self) -> Position {
        self.position.clone()
    }

    pub fn diagnostic(&mut self, level: Level, message: &str) {
        self.diagnostics.add(
            level,
            &format!(
                "{} at {}:{}:{}",
                message, self.position.file, self.position.line, self.position.pos
            ),
        );
    }

    pub fn specific(&mut self, result: LexResult, level: Level, message: &str) {
        self.diagnostics.add(
            level,
            &format!(
                "{} at {}:{}:{}",
                message, self.position.file, result.position.line, result.position.pos
            ),
        );
    }

    pub fn pos_diagnostic(&mut self, level: Level, pos: Position, message: &str) {
        self.diagnostics.add(
            level,
            &format!("{} at {}:{}:{}", message, pos.file, pos.line, pos.pos),
        );
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    pub fn mode(&self) -> Mode {
        self.mode.clone()
    }

    pub fn set_mode(&mut self, mode: Mode) {
        if mode == Mode::Formatting && self.peek_token("}") {
            self.mode = mode;
            self.peek = self.string();
        } else {
            self.mode = mode;
        }
    }

    #[allow(dead_code)]
    pub fn whitespace(&mut self) {
        while self.peek_token(" ") || self.peek_token("\t") {
            self.cont();
        }
    }

    fn none(&self) -> LexResult {
        LexResult {
            has: LexItem::None,
            position: Position {
                file: "".to_string(),
                line: 0,
                pos: 0,
            },
        }
    }

    /// parse a string for the lexer.
    fn string(&mut self) -> LexResult {
        let pos = self.position.clone();
        let mut res = String::new();
        while let Some(&c) = self.iter.peek() {
            if c == '"' {
                self.mode = Mode::Code;
                self.next_char();
                return LexResult::new(LexItem::CString(res), pos);
            }
            if c == '\\' {
                self.next_char();
                if let Some(&c) = self.iter.peek() {
                    match c {
                        '"' => res.push(c),
                        't' => res.push('\t'),
                        'r' => res.push('\r'),
                        'n' => res.push('\n'),
                        '\\' => res.push('\\'),
                        '\n' => break,
                        _ => {
                            self.err(Level::Error, "Unknown escape sequence");
                            res.push('?');
                        }
                    }
                } else {
                    break;
                }
            } else if c == '\n' {
                break;
            } else if c == '{' {
                self.next_char();
                if let Some('{') = self.iter.peek() {
                    res.push(c);
                } else {
                    self.mode = Mode::Formatting;
                    return LexResult::new(LexItem::CString(res), pos);
                }
            } else if c == '}' {
                self.next_char();
                if let Some('}') = self.iter.peek() {
                    res.push(c);
                } else {
                    self.err(Level::Warning, "Expected two '}' tokens");
                }
            } else {
                res.push(c);
            }
            self.next_char();
        }
        self.err(Level::Fatal, "String not correctly terminated");
        self.none()
    }

    fn next_char(&mut self) {
        self.iter.next();
        self.position.pos += 1;
    }

    fn get_identifier(&mut self) -> String {
        let mut string = String::new();
        while let Some(&ident) = self.iter.peek() {
            if ident.is_ascii_lowercase()
                || ident.is_ascii_uppercase()
                || ident.is_ascii_digit()
                || ident == '_'
                || (self.mode == Mode::Formatting && ident == '\\')
            {
                string.push(ident);
                self.next_char();
            } else {
                break;
            }
        }
        string
    }

    fn get_number(&mut self) -> String {
        let mut number = String::new();
        let mut hex = false;
        while let Some(&c) = self.iter.peek() {
            if c.is_ascii_digit() {
                number.push(c);
                self.next_char();
            } else if c == 'x' && !hex && number == "0" {
                hex = true;
                number.push(c);
                self.next_char();
            } else if hex && (('a'..='f').contains(&c) || ('A'..='F').contains(&c)) {
                number.push(c);
                self.next_char();
            } else {
                break;
            }
        }
        number
    }

    /// parse a number for the lexer.
    fn number(&mut self) -> LexResult {
        let pos = self.position.clone();
        let mut val = self.get_number();
        let mut f = false;
        if let Some('.') = self.iter.peek() {
            f = true;
            self.next_char();
            if let Some('.') = self.iter.peek() {
                self.next_char();
                self.link = self.memory.len();
                self.memory.push(LexResult::new(
                    LexItem::Token("..".to_string()),
                    pos.clone(),
                ));
                let res = if let Ok(r) = val.parse::<u64>() {
                    r
                } else {
                    self.err(Level::Error, "Problem parsing long");
                    0
                };
                return self.ret_number(res, pos, false);
            }
            val.push('.');
            let part = self.get_number();
            if part.is_empty() {
                self.err(Level::Error, "Problem parsing float");
                return self.none();
            }
            val += &part;
        }
        if let Some('e') = self.iter.peek() {
            f = true;
            val.push('e');
            self.next_char();
            if let Some('-') = self.iter.peek() {
                self.next_char();
                val.push('-');
            }
            let exp = self.get_number();
            if exp.is_empty() {
                self.err(Level::Error, "Problem parsing float");
                return self.none();
            }
            val += &exp;
        }
        if f {
            if let Some('f') = self.iter.peek() {
                self.next_char();
                if let Ok(r) = val.parse::<f32>() {
                    LexResult::new(LexItem::Single(r), pos)
                } else {
                    self.err(Level::Error, "Problem parsing single float");
                    LexResult::new(LexItem::Single(0.0), pos)
                }
            } else if let Ok(r) = val.parse::<f64>() {
                LexResult::new(LexItem::Float(r), pos)
            } else {
                self.err(Level::Error, "Problem parsing float");
                LexResult::new(LexItem::Float(0.0), pos)
            }
        } else if let Some(short) = val.strip_prefix("0x") {
            let res = if let Some(r) = hex_parse(short) {
                r
            } else {
                self.err(Level::Error, "Problem parsing hex number");
                0
            };
            self.ret_number(res, pos, false)
        } else if let Some(short) = val.strip_prefix("0b") {
            let res = if let Some(r) = bin_parse(short) {
                r
            } else {
                self.err(Level::Error, "Problem parsing binary number");
                0
            };
            self.ret_number(res, pos, false)
        } else if let Some(short) = val.strip_prefix("0o") {
            let res = if let Some(r) = oct_parse(short) {
                r
            } else {
                self.err(Level::Error, "Problem parsing octal number");
                0
            };
            self.ret_number(res, pos, false)
        } else if let Ok(r) = val.parse::<u64>() {
            self.ret_number(r, pos, val.starts_with('0'))
        } else {
            self.err(Level::Error, "Problem parsing number");
            self.ret_number(0, pos, false)
        }
    }

    fn ret_number(&mut self, r: u64, p: Position, start_zero: bool) -> LexResult {
        let max = i32::max as usize;
        if let Some('l') = self.iter.peek() {
            self.next_char();
            LexResult::new(LexItem::Long(r), p)
        } else if r > max as u64 {
            self.err(Level::Error, "Problem parsing integer");
            LexResult::new(LexItem::Integer(0, start_zero), p)
        } else {
            LexResult::new(LexItem::Integer(r as u32, start_zero), p)
        }
    }

    /// Create a lexer from a line iterator, useful for parsing text file content.
    pub fn lines(it: impl Iterator<Item = IoResult<String>> + 'static, filename: &str) -> Lexer {
        let mut l = Lexer::new(it, filename);
        l.cont();
        l
    }

    fn err(&mut self, level: Level, error: &str) {
        diagnostic!(self, level, "{error}");
    }

    /// debug feature to check the amount of currently in use links
    pub fn count_links(&self) -> u32 {
        return *self.links.borrow();
    }

    /// Return the currently found lexer element.
    pub fn peek(&self) -> LexResult {
        self.peek.clone()
    }

    pub fn peek_token(&self, token: &str) -> bool {
        self.peek.has == LexItem::Token(token.to_string())
    }

    fn end(&mut self) {
        self.peek = LexResult {
            has: LexItem::None,
            position: self.position.clone(),
        }
    }

    /// Continue the lexer to the next step.
    pub fn cont(&mut self) {
        let Some(n) = self.next() else {
            self.end();
            return;
        };
        let mut res = n;
        while res.has == LexItem::Token("//".to_string()) {
            while self.iter.peek().is_some() {
                self.iter.next();
            }
            let Some(n) = self.next() else {
                self.end();
                return;
            };
            res = n;
        }
        if self.link == self.memory.len() {
            if self.count_links() > 0 {
                self.memory.push(res.clone());
                self.link += 1;
            } else {
                self.memory.clear();
                self.link = 0;
            }
        }
        self.peek = res;
    }

    /// Create a link to the current lexer position, it can be used to revert to
    /// this position later.
    pub fn link(&mut self) -> Link {
        let cur: u32 = *self.links.borrow();
        self.links.replace(cur + 1);
        if self.memory.is_empty() {
            self.memory.push(self.peek.clone());
            self.link += 1;
        }
        Link {
            links: Rc::clone(&self.links),
            pos: self.link - 1,
        }
    }

    /// Reset to a previously made link position in the source.
    pub fn revert(&mut self, link: Link) {
        self.link = link.pos;
        drop(link);
        self.cont();
    }

    pub fn token(&mut self, token: &'static str) -> bool {
        if !self.has_token(token) {
            diagnostic!(self, Level::Error, "Expect token {}", token);
            false
        } else {
            true
        }
    }

    /// Shorthand test if the current element is a specific token and skip it if found.
    pub fn has_token(&mut self, token: &'static str) -> bool {
        if self.peek_token(token) {
            self.cont();
            true
        } else {
            false
        }
    }

    /// Shorthand test if the current element is a specific local keyword, so not one of the reserved
    pub fn has_keyword(&mut self, keyword: &'static str) -> bool {
        if self.peek.has == LexItem::Identifier(keyword.to_string()) {
            self.cont();
            true
        } else {
            false
        }
    }

    /// Shorthand test if the current element is a number and skip it if found.
    pub fn has_integer(&mut self) -> Option<u32> {
        if let LexItem::Integer(n, _) = self.peek().has {
            self.cont();
            Some(n)
        } else {
            None
        }
    }

    /// Shorthand test if the current element is a number and skip it if found.
    pub fn has_long(&mut self) -> Option<u64> {
        if let LexItem::Long(n) = self.peek().has {
            self.cont();
            Some(n)
        } else if let LexItem::Integer(n, _zero) = self.peek().has {
            self.cont();
            Some(n as u64)
        } else {
            None
        }
    }

    /// Shorthand test if the current element is a constant string and skip it if found.
    pub fn has_cstring(&mut self) -> Option<String> {
        if let LexItem::CString(n) = self.peek().has {
            self.cont();
            Some(n)
        } else {
            None
        }
    }

    /// Shorthand test if the current element is a float and skip it if found.
    pub fn has_float(&mut self) -> Option<f64> {
        if let LexItem::Float(n) = self.peek().has {
            self.cont();
            Some(n)
        } else {
            None
        }
    }

    /// Shorthand test if the current element is a float and skip it if found.
    pub fn has_single(&mut self) -> Option<f32> {
        if let LexItem::Single(n) = self.peek().has {
            self.cont();
            Some(n)
        } else {
            None
        }
    }

    /// Shorthand test if the current element is an identifier and skip it if found.
    pub fn has_identifier(&mut self) -> Option<String> {
        if let LexItem::Identifier(n) = self.peek().has {
            self.cont();
            Some(n)
        } else {
            None
        }
    }

    /// Create a lexer from a static string
    pub fn from_str(s: &str, filename: &str) -> Lexer {
        let mut v = Vec::new();
        for l in s.split('\n') {
            v.push(Ok(String::from(l)))
        }
        let mut res = Lexer::new(v.into_iter(), filename);
        res.cont();
        res
    }
}

#[cfg(test)]
mod test {
    fn test_id(lexer: &Lexer, id: &str) {
        assert_eq!(lexer.peek().has, LexItem::Identifier(String::from(id)));
    }

    fn links(lexer: &Lexer, nr: u32) {
        assert_eq!(lexer.count_links(), nr);
    }

    #[allow(unreachable_code)]
    fn array(lexer: &mut Lexer) -> Vec<LexItem> {
        let mut rest = Vec::new();
        rest.push(lexer.peek().has);
        loop {
            let Some(res) = lexer.next() else {
                break;
            };
            rest.push(res.has);
        }
        rest
    }

    use super::*;
    fn validate(s: &'static str, data: &[LexItem]) {
        let res = array(&mut Lexer::from_str(s, "validate"));
        assert_eq!(res, data)
    }

    #[cfg(test)]
    fn error(s: &'static str, err: &'static str) {
        let mut l = Lexer::from_str(s, "error");
        l.cont();
        assert_eq!(format!("{:?}", l.diagnostics), err.to_string())
    }

    #[cfg(test)]
    fn tokens(s: &'static str, t: &'static [&'static str]) {
        let mut data: Vec<LexItem> = Vec::new();
        for s in t {
            if s.chars().next().unwrap().is_ascii_digit() {
                if let Ok(res) = s.parse::<u32>() {
                    data.push(LexItem::Integer(res, false))
                } else {
                    panic!("Cannot parse {}", s)
                }
            } else if KEYWORDS.contains(s) || TOKENS.contains(s) {
                data.push(LexItem::Token(s.to_string()))
            } else {
                data.push(LexItem::Identifier(s.to_string()))
            }
        }
        assert_eq!(array(&mut Lexer::from_str(s, "tokens")), data)
    }

    #[test]
    fn test_lexer() {
        validate("1234", &[LexItem::Integer(1234, false)]);
        validate("0xaf", &[LexItem::Integer(0xaf, false)]);
        validate("1e2", &[LexItem::Float(100.0)]);
        validate(
            "1..4",
            &[
                LexItem::Integer(1, false),
                LexItem::Token("..".to_string()),
                LexItem::Integer(4, false),
            ],
        );
        tokens("=1+2", &["=", "1", "+", "2"]);
        tokens("=if 1 in a", &["=", "if", "1", "in", "a"]);
    }

    #[test]
    fn lexer_errors() {
        error("123.a", "[\"Error: Problem parsing float at error:1:5\"]");
        error("12. ", "[\"Error: Problem parsing float at error:1:4\"]");
        error("1.12ea", "[\"Error: Problem parsing float at error:1:6\"]");
        error(
            "123456789012345678901",
            "[\"Error: Problem parsing number at error:1:22\"]",
        );
        error(
            "\"1\\a2\"",
            "[\"Error: Unknown escape sequence at error:1:4\"]",
        );
        error(
            "\"\\",
            "[\"Fatal: String not correctly terminated at error:1:3\"]",
        );
        error(
            "\"1\\t2",
            "[\"Fatal: String not correctly terminated at error:1:6\"]",
        );
        error(
            "\"12\nss",
            "[\"Fatal: String not correctly terminated at error:1:4\"]",
        );
    }

    #[test]
    fn test_links() {
        let mut lex = Lexer::from_str("{num:1 + a*(2.0e2+= b )", "test_links");
        assert_eq!(lex.count_links(), 0);
        assert_eq!(lex.peek().has, LexItem::Token(String::from("{")));
        {
            lex.cont();
            test_id(&lex, "num");
            let l1 = lex.link();
            links(&lex, 1);
            test_id(&lex, "num");
            lex.cont();
            assert!(lex.has_token(":"));
            assert_eq!(lex.peek().has, LexItem::Integer(1, false));
            links(&lex, 1);
            lex.revert(l1);
            test_id(&lex, "num");
            links(&lex, 0);
        }
        links(&lex, 0);
        test_id(&lex, "num");
        lex.cont();
        links(&lex, 0);
        assert_eq!(lex.peek().has, LexItem::Token(":".to_string()));
        lex.mode = Mode::Code;
        assert!(lex.has_token(":"));
        if let Some(n) = lex.has_integer() {
            assert_eq!(n, 1);
        } else {
            panic!("Expected a number")
        }
        assert!(lex.has_token("+"));
        if let Some(n) = lex.has_identifier() {
            assert_eq!(n, "a");
        } else {
            panic!("Expected an identifier")
        }
        assert!(lex.has_token("*"));
        assert!(lex.has_token("("));
        if let LexResult {
            has: LexItem::Float(f),
            ..
        } = lex.peek()
        {
            assert_eq!(f, 200.0);
        } else {
            panic!("Expected a float")
        };
        lex.cont();
        assert!(lex.has_token("+="));
    }

    #[test]
    fn test_formats() {
        validate(
            "\"ab{{cd}}ef\"",
            &[LexItem::CString("ab{cd}ef".to_string())],
        );
        validate(
            "\"ab{c:d}ef\"",
            &[
                LexItem::CString("ab".to_string()),
                LexItem::Identifier("c".to_string()),
                LexItem::Token(":".to_string()),
                LexItem::Identifier("d".to_string()),
                LexItem::CString("ef".to_string()),
            ],
        );
    }
}
