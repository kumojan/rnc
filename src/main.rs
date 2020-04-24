use std::env;

const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
fn main() -> Result<(), &'static str> {
    match env::args().nth(1) {
        Some(x) => match tokenize(x) {
            None => Err("トークナイズできません。"),
            Some(list) => consume_list(list),
        },
        None => Err("引数の数が正しくありません。"),
    }?;
    println!("  ret");
    Ok(())
}

fn consume_list(mut list: Vec<Token>) -> Result<(), &'static str> {
    println!("{}", HEADER);
    list.reverse();
    let fstnum = expect_num(list.pop().unwrap())?;
    println!("  mov rax, {}", fstnum);
    loop {
        match list.pop().unwrap() {
            TkReserved(c) => {
                let num = expect_num(list.pop().unwrap())?;
                match c {
                    '+' => println!("  add rax, {}", num),
                    '-' => println!("  sub rax, {}", num),
                    _ => return Err("unknown"),
                }
            }
            TkEOF => return Ok(()),
            _ => return Err("didn't expect a number"),
        }
    }
}

use crate::Token::*;

#[derive(Debug)]
enum Token {
    TkReserved(char),
    TkNum(u32),
    TkEOF,
}

fn tokenize(mut code: String) -> Option<Vec<Token>> {
    let mut list = vec![];
    let mut numvec: Vec<u32> = vec![];
    code.push(' '); // numvecを回収させるために、末尾に空白追加(なんか嫌だけど)
    for c in code.chars() {
        if c.is_ascii_digit() {
            numvec.push(c.to_digit(10).unwrap());
            continue;
        } else if !numvec.is_empty() {
            let tk = TkNum(numvec.iter().fold(0, |acc, x| acc * 10 + x));
            list.push(tk);
            numvec = vec![];
        }
        if c == '+' || c == '-' {
            list.push(TkReserved(c));
            continue;
        }
        if c.is_whitespace() {
            continue;
        }
        return None;
    }
    list.push(TkEOF);
    Some(list)
}

fn expect_num(tk: Token) -> Result<u32, &'static str> {
    match tk {
        TkNum(x) => Ok(x),
        _ => Err("expected number but got sth else"),
    }
}
