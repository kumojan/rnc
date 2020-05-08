use std::env;
// use std::str;
mod codegen;
mod err;
mod parse;
mod tokenize;
use crate::codegen::{code_gen, print_graph};
use crate::err::*;
use crate::parse::*;
use crate::tokenize::*;

fn main() -> Result<(), CompileError> {
    // Result型を返すことで、, エラー終了時に終了ステータス1となる。
    //
    match env::args().nth(1) {
        Some(code) => match compile(&code) {
            Err(EnumError::Tokenize { pos }) => error_at(&code, pos, "uknown token"),
            Err(EnumError::Parse { pos, msg }) => {
                error_at(&code, pos, format!("parse failed: {}", msg))
            }
            _ => Ok(()),
        },
        None => error("input needed"),
    }
}

fn compile(code: &String) -> Result<(), EnumError> {
    let token_list = tokenize(code)?;
    // println!("{:?}", token_list);
    let mut parser = Parser::new(token_list);
    let nodes = parser.program()?;
    print_graph(&nodes);
    code_gen(nodes, parser.varoffset);
    Ok(())
}
