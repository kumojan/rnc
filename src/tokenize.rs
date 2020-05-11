use crate::tokenize::TokenKind::*;
use std::collections::{HashMap, VecDeque};
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
    TkPunct(String),
    TkResWord(String),
    TkIdent(String),
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
    fn new_res(s: String, pos: usize) -> Self {
        Self {
            kind: TkReserved(s),
            pos,
        }
    }
    fn new_ident(s: String, pos: usize) -> Self {
        Self {
            kind: TkIdent(s),
            pos,
        }
    }
    fn new_punct(s: String, pos: usize) -> Self {
        Self {
            kind: TkPunct(s),
            pos,
        }
    }
    fn new_res_word(s: String, pos: usize) -> Self {
        Self {
            kind: TkResWord(s),
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
// このformaterを書き換えてcodeを挿入したら、自動でメッセージ出力できそうな気がする。
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
        (match l {
            2 => ["if"].contains(&&s[..]),
            3 => ["for"].contains(&&s[..]),
            4 => ["else", "while"].contains(&&s[..]),
            6 => ["return"].contains(&&s[..]),
            _ => unimplemented!(),
        }) && !is_alnum(&self.peek_char(l))
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
        if "+-*/()<>=;".find(c).is_some() {
            self.pos += 1;
            return Some(c.to_string());
        }
        None
    }
    fn read_word(&mut self) -> Option<String> {
        for l in &[2, 3, 4, 6] {
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
    pub fn tokenize(&mut self) -> Result<VecDeque<Token>, TokenizeError> {
        let mut list: VecDeque<Token> = VecDeque::new();
        while !self.is_at_end() {
            if self.read_whitespace() {
                continue;
            }
            if let Some(s) = self.read_punct() {
                list.push_back(Token::new_punct(s, self.pos));
                continue;
            }
            if let Some(s) = self.read_word() {
                list.push_back(Token::new_res_word(s, self.pos));
                continue;
            }
            if let Some(n) = self.read_num() {
                list.push_back(Token::new_num(n, self.pos));
                continue;
            }
            if let Some(s) = self.read_ident() {
                list.push_back(Token::new_ident(s, self.pos));
                continue;
            }
            return Err(self.pos)?;
        }
        list.push_back(Token {
            kind: TkEOF,
            pos: self.code.len(),
        });
        Ok(list)
    }
}

///
/// トークン列を生成
///
pub fn tokenize(code: &String) -> Result<VecDeque<Token>, TokenizeError> {
    // const two_char_tokens: [&'static [u8; 2]; 1] = [b"if"];
    // const three_char_tokens: [&'static [u8; 3]; 1] = [b"for"];
    let code: Vec<char> = code.chars().collect();
    let mut list: VecDeque<Token> = VecDeque::new();
    let mut pos = 0;
    while pos < code.len() {
        // 空白記号を飛ばす
        if code[pos].is_whitespace() {
            pos += 1;
            continue;
        }
        // 記号読み取り
        // 2文字読み取り
        if pos < code.len() - 1 {
            let s: String = code[pos..pos + 2].to_vec().iter().collect();
            if ["==", "!=", "<=", ">="].contains(&&s[..]) {
                let tk = Token::new_res(s, pos);
                list.push_back(tk);
                pos += 2;
                continue;
            }
        }
        // 1文字読み取り
        if "+-*/()<>=;".find(code[pos]).is_some() {
            list.push_back(Token::new_res(code[pos].to_string(), pos));
            pos += 1;
            continue;
        }

        // 文字列読み取り
        // return読み取り
        if pos + 6 < code.len() {
            let s: String = code[pos..pos + 6].to_vec().iter().collect();
            if "return" == &s && !is_alnum(&code[pos + 6]) {
                list.push_back(Token {
                    kind: TkReturn,
                    pos,
                });
                pos += 6;
                continue;
            }
        }
        // else読み取り
        if pos + 4 < code.len() {
            let s: String = code[pos..pos + 4].to_vec().iter().collect();
            if "else" == &s && !is_alnum(&code[pos + 4]) {
                list.push_back(Token {
                    kind: TkReserved(s),
                    pos,
                });
                pos += 4;
                continue;
            }
        }
        // for読み取り
        if pos + 3 < code.len() {
            let s: String = code[pos..pos + 3].to_vec().iter().collect();
            if "for" == &s && !is_alnum(&code[pos + 3]) {
                list.push_back(Token {
                    kind: TkReserved(s),
                    pos,
                });
                pos += 3;
                continue;
            }
        }
        // if読み取り
        if pos + 2 < code.len() {
            let s: String = code[pos..pos + 2].to_vec().iter().collect();
            if "for" == &s && !is_alnum(&code[pos + 2]) {
                list.push_back(Token {
                    kind: TkReserved(s),
                    pos,
                });
                pos += 2;
                continue;
            }
        }

        // 数字読み取り
        let n: String = code[pos..]
            .iter()
            .take_while(|c| c.is_ascii_digit())
            .collect();
        if n.len() > 0 {
            pos += n.len();
            list.push_back(Token::new_num(n.parse().unwrap(), pos));
            continue;
        }
        // 変数名読み取り
        let s: String = code[pos..].iter().take_while(|c| is_alnum(&c)).collect();
        if s.len() > 0 {
            pos += s.len();
            list.push_back(Token::new_ident(s, pos));
            continue;
        }
        return Err(pos)?;
    }
    list.push_back(Token {
        kind: TkEOF,
        pos: code.len(),
    });
    Ok(list)
}
