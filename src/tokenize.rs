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
    TkNum(u32),
    TkReturn,
    TkEOF,
}

/// 初期値EOF
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

///
/// トークン列を生成
///
pub fn tokenize(code: &String) -> Result<VecDeque<Token>, TokenizeError> {
    let mut list: VecDeque<Token> = VecDeque::new();
    // let code = &format!("{} ", code)[..]; // numvecを回収させるために、末尾に空白追加(なんか嫌だけど)
    let mut pos = 0;
    while pos < code.len() {
        // return読み取り
        if pos + 6 < code.len() {
            let b = &code.as_bytes()[pos..pos + 6];
            if b"return" == b && !is_alnum(&code.chars().nth(pos + 6).unwrap()) {
                list.push_back(Token {
                    kind: TkReturn,
                    pos,
                });
                pos += 6;
                continue;
            }
        }
        // 2文字読み取り
        if pos < code.len() - 1 {
            let x = &code.as_bytes()[pos..pos + 2]; // バイトでみた方が効率よさそう。
            if [b"==", b"!=", b"<=", b">="].iter().any(|b| b == &x) {
                let s = String::from_utf8(x.to_vec()).unwrap(); // "==", "!=", "<=", ">=" のどれかなので大丈夫
                let tk = Token::new_res(s, pos);
                list.push_back(tk);
                pos += 2;
                continue;
            }
        }
        // 1文字読み取り
        let c = code.chars().nth(pos).unwrap();
        if "+-*/()<>=;".find(c).is_some() {
            list.push_back(Token::new_res(c.to_string(), pos));
            pos += 1;
            continue;
        }

        // 数字読み取り
        let n: String = code
            .chars()
            .skip(pos)
            .take_while(|c| c.is_ascii_digit())
            .collect();
        if n.len() > 0 {
            pos += n.len();
            list.push_back(Token::new_num(n.parse().unwrap(), pos));
            continue;
        }
        // 変数名読み取り
        let s: String = code.chars().skip(pos).take_while(is_alnum).collect();
        let sl = s.len();
        if s.len() > 0 {
            list.push_back(Token::new_ident(s, pos));
            pos += sl;
            continue;
        }
        if c.is_whitespace() {
            pos += 1;
            continue;
        }
        return Err(pos)?;
    }
    list.push_back(Token {
        kind: TkEOF,
        pos: code.len(),
    });
    // list.reverse();
    Ok(list)
}
