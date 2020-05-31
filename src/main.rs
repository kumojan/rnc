use std::env;
extern crate getopts;
use getopts::Options;

mod codegen;
mod err;
mod parse;
mod tokenize;
mod r#type;
use crate::codegen::{code_gen, print_graph};
use crate::err::*;
use crate::parse::*;
use crate::tokenize::*;

fn main() -> Result<(), String> {
    // 実験
    // for i in 0..256 {
    //     println!(
    //         "{} {:?}",
    //         (i as u8 as char),
    //         (i as u8 as char).to_string().as_bytes()
    //     );
    // }
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
            let code = open(&filename).map_err(|_| format!("cannot open file {}", filename))?;
            match compile(&code, matches.opt_present("t"), matches.opt_present("g")) {
                Err(CompileError::Tokenize { pos, msg }) => error_at(&code, pos, msg),
                Err(CompileError::Parse { pos, msg }) => {
                    error_at(&code, pos, format!("parse failed: {}", msg))
                }
                Err(CompileError::CodeGen { msg }) => {
                    error(format!("codegen failed with node: {}", msg))
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
    let parser = Parser::new(token_list);
    let (functions, globals, string_literals) = parser.program()?;
    if print_graph_ {
        print_graph(&functions);
    }
    code_gen(functions, globals, string_literals)?;
    Ok(())
}

use std::fs::File;
use std::io::prelude::*;
fn open(filename: &String) -> std::io::Result<String> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
