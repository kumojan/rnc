use crate::tokenize::TokenKind::*;
use std::collections::VecDeque;
use std::fmt;
///
/// トークナイザ
/// 字句解析
/// Lexical Analysis
/// トークナイザは、空白で区切る、記号は1文字ずつなどのルールで、入力文を
/// トークン列に分けていく
/// エラーメッセージとしては、予期せぬ記号のみ
///
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    TkReserved(String),
    TkIdent(String),
    TkString(String),
    TkNum(u32),
    TkReturn,
    TkEOF,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}
impl Token {
    fn new_num(val: u32, pos: usize) -> Self {
        Self {
            kind: TkNum(val),
            pos,
        }
    }
    fn new_ident(s: String, pos: usize) -> Self {
        Self {
            kind: TkIdent(s),
            pos,
        }
    }
    fn new_reserved(s: String, pos: usize) -> Self {
        Self {
            kind: TkReserved(s),
            pos,
        }
    }
    fn new_string(s: String, pos: usize) -> Self {
        Self {
            kind: TkString(s),
            pos,
        }
    }
}

///
/// トークナイズエラー
///
#[derive(Debug, Default)]
pub struct TokenizeError {
    pub pos: usize,
}
// このformaterを書き換えてcodeを挿入したら、自動でメッセージ出力できそうな気がする。無理か...。
impl fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid token at {}", self.pos)
    }
}
impl From<usize> for TokenizeError {
    fn from(pos: usize) -> Self {
        Self { pos }
    }
}

fn is_alnum(c: &char) -> bool {
    ('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..='9').contains(c) || *c == '_'
}

pub struct Lexer {
    code: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(code: &String) -> Self {
        Self {
            code: code.chars().collect(),
            pos: 0,
        }
    }
    fn check_res_word(&self, s: &String, l: usize) -> bool {
        !is_alnum(&self.peek_char(l))
            && match l {
                2 => ["if"].contains(&&s[..]),
                3 => ["for", "int"].contains(&&s[..]),
                4 => ["else", "char"].contains(&&s[..]),
                5 => ["while"].contains(&&s[..]),
                6 => ["return", "sizeof"].contains(&&s[..]),
                _ => unimplemented!(),
            }
    }
    fn is_at_end(&self) -> bool {
        self.pos >= self.code.len()
    }
    fn peek_char(&self, n: usize) -> char {
        self.code[self.pos + n]
    }
    fn peek_str(&self, n: usize) -> String {
        self.code[self.pos..self.pos + n].to_vec().iter().collect()
    }
    fn read_whitespace(&mut self) -> bool {
        if self.peek_char(0).is_whitespace() {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn read_num(&mut self) -> Option<u32> {
        let n: String = self.code[self.pos..]
            .iter()
            .take_while(|c| c.is_ascii_digit())
            .collect();
        if n.len() > 0 {
            self.pos += n.len();
            Some(n.parse().unwrap())
        } else {
            None
        }
    }
    fn read_ident(&mut self) -> Option<String> {
        let s: String = self.code[self.pos..]
            .iter()
            .take_while(|c| is_alnum(&c))
            .collect();
        if s.len() > 0 {
            self.pos += s.len();
            Some(s)
        } else {
            None
        }
    }
    fn read_punct(&mut self) -> Option<String> {
        // 記号読み取り
        // 2文字
        if self.pos < self.code.len() - 1 {
            let s = self.peek_str(2);
            if ["==", "!=", "<=", ">="].contains(&&s[..]) {
                self.pos += 2;
                return Some(s);
            }
        }
        // 1文字読み取り
        let c = self.peek_char(0);
        if "+-*/(){}<>=;,*&[]".find(c).is_some() {
            self.pos += 1;
            return Some(c.to_string());
        }
        None
    }
    fn read_word(&mut self) -> Option<String> {
        for l in &[2, 3, 4, 5, 6] {
            if self.pos + l < self.code.len() {
                let s = self.peek_str(*l);
                if self.check_res_word(&s, *l) {
                    self.pos += l;
                    return Some(s);
                }
            }
        }
        None
    }
    fn read_string(&mut self) -> Option<String> {
        if self.peek_char(0) == '"' {
            self.pos += 1;
            let s: String = self.code[self.pos..]
                .iter()
                .take_while(|c| **c != '"')
                .collect();
            self.pos += s.len() + 1;
            return Some(s);
        }
        None
    }
    pub fn tokenize(&mut self) -> Result<VecDeque<Token>, TokenizeError> {
        let mut list: VecDeque<Token> = VecDeque::new();
        while !self.is_at_end() {
            if self.read_whitespace() {
                continue;
            } else if let Some(s) = self.read_punct() {
                list.push_back(Token::new_reserved(s, self.pos));
            } else if let Some(s) = self.read_word() {
                list.push_back(Token::new_reserved(s, self.pos));
            } else if let Some(n) = self.read_num() {
                list.push_back(Token::new_num(n, self.pos));
            } else if let Some(s) = self.read_ident() {
                list.push_back(Token::new_ident(s, self.pos));
            } else if let Some(s) = self.read_string() {
                list.push_back(Token::new_string(s, self.pos));
            } else {
                return Err(self.pos)?;
            }
        }
        list.push_back(Token {
            kind: TkEOF,
            pos: self.code.len(),
        });
        Ok(list)
    }
}
