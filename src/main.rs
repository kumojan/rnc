use std::env;
extern crate getopts;
use getopts::Options;

mod codegen;
mod err;
mod parse;
mod tokenize;
use crate::codegen::{code_gen, print_graph};
use crate::err::*;
use crate::parse::*;
use crate::tokenize::*;

fn main() -> Result<(), CompileError> {
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
    //
    match env::args().last() {
        Some(code) => match compile(&code, matches.opt_present("t"), matches.opt_present("g")) {
            Err(EnumError::Tokenize { pos }) => error_at(&code, pos, "uknown token"),
            Err(EnumError::Parse { pos, msg }) => {
                error_at(&code, pos, format!("parse failed: {}", msg))
            }
            Err(EnumError::CodeGen { msg }) => error(format!("codegen failed with node: {}", msg)),
            _ => Ok(()),
        },
        None => error("input needed"),
    }
}

fn compile(code: &String, print_tklist: bool, print_graph_: bool) -> Result<(), EnumError> {
    let token_list = Lexer::new(code).tokenize()?;
    if print_tklist {
        println!("{:?}", token_list);
    }
    let mut parser = Parser::new(token_list);
    let nodes = parser.program()?;
    if print_graph_ {
        print_graph(&nodes);
    }
    code_gen(nodes, parser.varoffset)?;
    Ok(())
}
