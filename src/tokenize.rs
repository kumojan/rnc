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
#[derive(Clone, PartialEq, Debug)]
pub enum TokenKind {
    TkReserved(String),
    TkIdent(String),
    TkString(CString),
    TkNum(usize),
    TkChar(u8),
    TkEOF,
}
#[derive(Clone, PartialEq)]
pub struct CString(pub Vec<u8>); // Vec<u8>のエイリアス
impl fmt::Debug for CString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.0).escape_debug())
    }
}
impl Default for TokenKind {
    fn default() -> Self {
        Self::TkEOF
    }
}

#[derive(Debug, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
    pub len: usize,
    pub line_no: usize,
    pub byte_len: usize,
}
impl Token {
    fn new_num(val: usize, pos: usize, len: usize) -> Self {
        Self {
            kind: TkNum(val),
            pos,
            len,
            ..Default::default()
        }
    }
    fn new_ident(s: String, pos: usize) -> Self {
        let len = s.len();
        Self {
            kind: TkIdent(s),
            pos,
            len,
            ..Default::default()
        }
    }
    fn new_reserved(s: String, pos: usize) -> Self {
        let len = s.len();
        Self {
            kind: TkReserved(s),
            pos,
            len,
            ..Default::default()
        }
    }
    fn new_string(s: Vec<u8>, pos: usize, len: usize) -> Self {
        Self {
            kind: TkString(CString(s)),
            pos,
            len,
            ..Default::default()
        }
    }
    fn new_char(c: u8, pos: usize) -> Self {
        Self {
            kind: TkChar(c),
            pos,
            len: 1,
            ..Default::default()
        }
    }
}

///
/// トークナイズエラー
///
#[derive(Debug, Default)]
pub struct TokenizeError {
    pub msg: String,
    pub pos: usize,
}
impl fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid token at {}", self.pos)
    }
}
impl From<usize> for TokenizeError {
    fn from(pos: usize) -> Self {
        Self::new("unknown token", pos)
    }
}
impl TokenizeError {
    fn new(msg: &str, pos: usize) -> Self {
        Self {
            msg: msg.to_owned(),
            pos,
        }
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
    fn check_res_word(&self, s: &str, l: usize) -> bool {
        !is_alnum(&self.peek_char(l))
            && match l {
                2 => ["if"].contains(&s),
                3 => ["for", "int"].contains(&s),
                4 => ["else", "char", "long", "void", "enum"].contains(&s),
                5 => ["while", "union", "short", "_Bool"].contains(&s),
                6 => ["return", "sizeof", "struct", "static"].contains(&s),
                7 => ["typedef"].contains(&s),
                _ => unimplemented!(),
            }
    }
    fn is_at_end(&self) -> bool {
        self.pos >= self.code.len()
    }
    fn consume(&mut self) -> char {
        let c = self.code[self.pos];
        self.pos += 1;
        c
    }
    /// 現在位置からn文字目の文字を読み取る
    fn peek_char(&self, n: usize) -> char {
        self.code[self.pos + n]
    }
    /// 現在位置からn文字読み取る
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
    /// 読み取ったらSome(number), 読み取らなければNone
    fn read_num(&mut self) -> Result<Option<usize>, TokenizeError> {
        let base = match self.peek_char(0) {
            '0' => match self.peek_char(1) {
                'b' | 'B' => 2,
                'x' | 'X' => 16,
                _ => 8,
            },
            '1'..='9' => 10,
            _ => return Ok(None),
        };
        if base == 2 || base == 16 {
            self.pos += 2;
        }
        let n: String = self.code[self.pos..]
            .iter()
            .take_while(|c| {
                ('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..='9').contains(c)
            })
            .collect();
        if n.len() > 0 {
            self.pos += n.len();
            usize::from_str_radix(&n, base).map(Some).ok() // errorはNoneになる
        } else {
            None // error
        }
        .ok_or(TokenizeError::new(
            &format!("expected number of base {}", base),
            self.pos,
        ))
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
            if [
                "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--", "%=", "&=", "|=",
                "^=", "&&", "||",
            ]
            .contains(&&s[..])
            {
                self.pos += 2;
                return Some(s);
            }
        }
        // 1文字読み取り
        let c = self.peek_char(0);
        if "+-*/(){}<>=;,*&[].\'!~%|^".find(c).is_some() {
            self.pos += 1;
            return Some(c.to_string());
        }
        None
    }
    fn read_word(&mut self) -> Option<String> {
        for l in &[2, 3, 4, 5, 6, 7] {
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
    fn read_escape_char(&mut self) -> Result<u8, TokenizeError> {
        Ok(match self.consume() {
            'a' => 7,
            'b' => 8,
            't' => 9,
            'n' => 10,
            'v' => 11,
            'f' => 12,
            'r' => 13,
            'e' => 27,
            'x' | 'X' => self.read_hexadecimal()?,
            s => {
                let c = s.to_string().as_bytes().to_owned();
                if c.len() > 1 {
                    Err(TokenizeError::new("unknown escape sequence", self.pos))?
                }
                c[0]
            }
        })
    }
    fn read_char(&mut self) -> Result<Option<u8>, TokenizeError> {
        if self.peek_char(0) == '\'' {
            self.pos += 1;
            let c = self.consume().to_string().as_bytes().to_owned();
            if !c.len() == 1 {
                Err(TokenizeError::new(
                    "char literal accepts only one byte chars",
                    self.pos,
                ))?;
            }
            let mut c = c[0];
            if c as char == '\\' {
                c = self.read_escape_char()?;
            }
            if self.consume() != '\'' {
                Err(TokenizeError::new("char literal too long", self.pos))?;
            }
            Ok(Some(c))
        } else {
            Ok(None)
        }
    }

    fn read_octal(&mut self) -> Option<u8> {
        let s: String = self.code[self.pos..]
            .iter()
            .take(3) // octalは最大3桁しか読まない
            .take_while(|c| ('0'..='7').contains(c))
            .collect();
        self.pos += s.len();
        if s.len() > 0 {
            // literalでなければ、castしてもpanicしない(上位ビット切り捨て)
            Some(u32::from_str_radix(&s, 8).unwrap() as u8)
        } else {
            None
        }
    }
    fn read_hexadecimal(&mut self) -> Result<u8, TokenizeError> {
        let s: String = self.code[self.pos..]
            .iter()
            .take_while(|c| {
                // hexadecimalは何桁でも読む
                ('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..='9').contains(c)
            })
            .collect();
        self.pos += s.len();
        if s.len() > 0 {
            // rustのchar, Stringは文字コード外の文字列を受け付けないので、
            // 例えば、169 as char は ©という記号、ないし [194, 169] というバイト列になってしまう。
            // 一方cはcharはただのu8なので、169はそのまま169になる
            // この問題を回避するには、String LiteralをrustのStringとして保持するのをやめてVec<u8>
            // などとして持つ必要がある。 => 修正した
            Ok(u32::from_str_radix(&s, 16).unwrap() as u8)
        } else {
            Err(TokenizeError::new("expected hex digits", self.pos))
        }
    }
    fn read_string(&mut self) -> Result<Option<Vec<u8>>, TokenizeError> {
        if self.peek_char(0) == '"' {
            let start_pos = self.pos;
            self.pos += 1;
            let mut v = Vec::new();
            while !self.is_at_end() {
                match self.consume() {
                    '"' => {
                        return Ok(Some(v));
                    }
                    '\\' => {
                        if let Some(o) = self.read_octal() {
                            v.push(o)
                        } else {
                            if self.is_at_end() {
                                break;
                            }
                            v.push(self.read_escape_char()?);
                        }
                    }
                    // charを直接bytesに変換できないのだろうか...。
                    c => v.extend_from_slice(c.to_string().as_bytes()),
                }
            }
            Err(TokenizeError::new("unclosed string literal", start_pos))
        } else {
            Ok(None)
        }
    }
    fn read_comment(&mut self) -> Result<bool, TokenizeError> {
        if self.pos == self.code.len() - 1 {
            // 残り1文字の時
            return Ok(false);
        }
        if &self.peek_str(2) == "//" {
            self.pos += 2;
            self.pos += self.code[self.pos..]
                .iter()
                .position(|c| c == &'\n')
                .unwrap()
                + 1;
            return Ok(true);
        } else if &self.peek_str(2) == "/*" {
            let start_pos = self.pos;
            self.pos += 2;
            self.pos += self.code[self.pos..]
                .iter()
                .zip(self.code[self.pos + 1..].iter())
                .position(|c| c == (&'*', &'/'))
                .ok_or(TokenizeError::new("unclosed block comment", start_pos))?
                + 2;
            return Ok(true);
        }
        Ok(false)
    }
    pub fn tokenize(&mut self) -> Result<VecDeque<Token>, TokenizeError> {
        let mut list: VecDeque<Token> = VecDeque::new();
        while !self.is_at_end() {
            let tk_head = self.pos;
            if self.read_whitespace() || self.read_comment()? {
                continue;
            } else if let Some(c) = self.read_char()? {
                list.push_back(Token::new_char(c, tk_head));
            } else if let Some(s) = self.read_punct() {
                list.push_back(Token::new_reserved(s, tk_head));
            } else if let Some(s) = self.read_word() {
                list.push_back(Token::new_reserved(s, tk_head));
            } else if let Some(n) = self.read_num()? {
                list.push_back(Token::new_num(n, tk_head, self.pos - tk_head));
            } else if let Some(s) = self.read_ident() {
                list.push_back(Token::new_ident(s, tk_head));
            } else if let Some(s) = self.read_string()? {
                list.push_back(Token::new_string(s, tk_head, self.pos - tk_head));
            } else {
                return Err(self.pos)?;
            }
        }
        list.push_back(Token {
            kind: TkEOF,
            pos: self.code.len(),
            ..Default::default()
        });
        // 行番号を計算してトークンに付加
        let mut head = 0;
        let mut line_no = 0;
        let mut byte_len = 0;
        for mut tk in list.iter_mut() {
            byte_len += self.code[head..tk.pos]
                .iter()
                .map(|c| c.len_utf8())
                .sum::<usize>();
            line_no += self.code[head..tk.pos]
                .iter()
                .filter(|c| **c == '\n')
                .count();
            tk.line_no = line_no;
            tk.byte_len = byte_len;
            head = tk.pos;
        }
        Ok(list)
    }
}
