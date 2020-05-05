use crate::parse::Node;
use crate::parse::NodeKind::*;

fn gen(node: &Node) {
    if let Node::Leaf { val } = node {
        println!("  push {}", val);
    } else if let Node::Bin { kind, lhs, rhs } = node {
        gen(lhs);
        gen(rhs);
        println!("  pop rdi");
        println!("  pop rax");
        let code = match kind {
            NdAdd => "  add rax, rdi",
            NdSub => "  sub rax, rdi",
            NdMul => "  imul rax, rdi",
            NdDiv => "  cqo\n  idiv rdi",
            NdEq => "  cmp rax, rdi\n  sete al\n  movzb rax, al", // 最後のmovzbは、raxの上位56を削除して、下位8ビットにalを入れるということだろう。
            NdNeq => "  cmp rax, rdi\n  setne al\n  movzb rax, al",
            NdLt => "  cmp rax, rdi\n  setl al\n  movzb rax, al",
            NdLe => "  cmp rax, rdi\n  setle al\n  movzb rax, al",
            _ => unimplemented!(),
        };
        println!("{}", code);
        println!("  push rax")
    }
    println!("");
}
pub fn code_gen(node: &Node) {
    const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
    println!("{}", HEADER);
    gen(node);
    println!("  pop rax");
    println!("  ret");
}

use std::fs;
use std::io;
use std::io::{BufWriter, Write};
pub fn print_graph(node: &Node) -> io::Result<()> {
    let mut f = BufWriter::new(fs::File::create("graph.dot").unwrap());
    f.write(
        b"digraph {
node [
style = \"filled\",
fontsize = 16,
fillcolor = \"green\",
];
",
    )?;
    f.write(graph_gen(node, "root".to_owned()).as_bytes())?;
    f.write(b"}\n")?;
    Ok(())
}
pub fn graph_gen(node: &Node, name: String) -> String {
    if let Node::Leaf { val } = node {
        format!("{} [label=\"{}\"];\n", name, val)
    } else if let Node::Bin { kind, lhs, rhs } = node {
        let mut s = format!("{} [label=\"{:?}\"];\n", name, kind);
        let left = format!("{}l", name);
        let right = format!("{}r", name);
        s = s + &graph_gen(lhs, left.clone()) + &graph_gen(rhs, right.clone());
        s = s + &format!("{} -> {} [];\n{} -> {} [];\n", name, left, name, right);
        s
    } else {
        unimplemented!();
    }
}
