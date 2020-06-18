use std::env;
extern crate getopts;
use getopts::Options;

mod codegen;
mod err;
mod parse;
mod tokenize;
mod r#type;
mod util;
use crate::codegen::{code_gen, print_graph};
use crate::err::*;
use crate::parse::*;
use crate::tokenize::*;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optflag("t", "", "print token list");
    opts.optflag("g", "", "print node graph");
    // opts.optopt("c", "", "code to compile", "CODE");
    // 最初(プログラム名)と最後(入力)は無視する
    let matches = match opts.parse(&args[1..args.len()]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };
    // Result型を返すことで、, エラー終了時に終了ステータス1となる。
    match env::args().last() {
        Some(filename) => {
            println!(".file 1 \"{}\"", filename);
            let code = open(&filename).map_err(|_| format!("cannot open file {}", filename))?;
            match compile(&code, matches.opt_present("t"), matches.opt_present("g")) {
                Err(CompileError::Tokenize { pos, msg }) => error_at(&code, pos, msg, &filename),
                Err(CompileError::Parse { pos, msg }) => {
                    error_at(&code, pos, format!("parse failed: {}", msg,), &filename)
                }
                Err(CompileError::CodeGen { msg, pos }) => {
                    error_at(&code, pos, format!("codegen failed: {}", msg,), &filename)
                }
                _ => Ok(()),
            }
        }
        None => error("input needed"),
    }
}

fn compile(code: &String, print_tklist: bool, print_graph_: bool) -> Result<(), CompileError> {
    let token_list = Lexer::new(code).tokenize()?;
    if print_tklist {
        for tk in &token_list {
            println!("{:?}", tk);
        }
    }
    let mut parser = Parser::new(token_list);
    parser.code_lines = code.split('\n').collect();
    parser.code = &code;
    let (functions, globals, string_literals, token_list) = parser.program()?;
    if print_graph_ {
        print_graph(&functions);
    }
    code_gen(functions, globals, string_literals, token_list)?;
    Ok(())
}

use std::fs::File;
use std::io::prelude::*;
fn open(filename: &String) -> std::io::Result<String> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    if contents.chars().last() != Some('\n') {
        contents.push('\n');
    }
    Ok(contents)
}
