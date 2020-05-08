use crate::parse::Node;
use crate::parse::NodeKind::*;

fn gen_lval(node: &Node) {
    println!("  mov rax, rbp");
    println!("  sub rax, {}", node.offset());
    println!("  push rax\n");
}
fn gen(node: &Node) {
    if let Some(val) = node.if_num() {
        println!("  push {}", val);
    } else if let Node::Unary {
        kind: NdReturn,
        next,
    } = node
    {
        gen(next);
        println!("  pop rax");
        println!("  mov rsp, rbp"); // スタックをrbpまで戻し
        println!("  pop rbp"); // 以前のrbpがそこにあるので回収し、
        println!("  ret"); // 関数を抜ける
    } else if let Node::Leaf { kind: NdLvar, .. } = node {
        gen_lval(node);
        println!("  pop rax");
        println!("  mov rax, [rax]");
        println!("  push rax");
    } else if let Node::Bin {
        kind: NdAssign,
        lhs,
        rhs,
    } = node
    {
        gen_lval(lhs);
        gen(rhs);
        println!("  pop rdi");
        println!("  pop rax");
        println!("  mov [rax], rdi");
        println!("  push rdi");
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
    } else {
        unimplemented!();
    }
    println!("");
}
pub fn code_gen(nodes: Vec<Node>, varoffset: usize) {
    const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
    // まず現在の関数のrbpをスタックにpush(戻る場所)、次に現在のrsp(スタックポインタ)の値をrbpに格納し、rspを使用する変数分ずらす
    println!("{}", HEADER);
    println!("  push rbp"); // まず現在の関数のrbpをスタックにpush(戻る場所?)
    println!("  mov rbp, rsp"); // 次にrbpに現在のrspを入れる。rspは常にスタックの1番下を指していて、rbpもそこを指すようになる
    println!("  sub rsp, {}\n", varoffset); // rspを変数分ずらす。この時スタックは自動的にずれる。(26回pushするのと同じ?)
    for node in &nodes {
        gen(node);
        println!("  pop rax");
    }
    println!("  ret");
}

use std::fs;
use std::io;
use std::io::{BufWriter, Write};
pub fn print_graph(nodes: &Vec<Node>) -> io::Result<()> {
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
    for (i, node) in nodes.iter().enumerate() {
        f.write(graph_gen(node, format!("root{}", i)).as_bytes())?;
    }
    f.write(b"}\n")?;
    Ok(())
}
pub fn graph_gen(node: &Node, name: String) -> String {
    if let Node::Leaf {
        kind,
        val,
        name: _name,
        offset,
    } = node
    {
        match kind {
            NdNum => format!("{} [label=\"{:?} {}\"];\n", name, kind, val),
            NdLvar => format!(
                "{} [label=\"{:?} {} ofs:{}\"];\n",
                name, kind, _name, offset
            ),
            _ => panic!(),
        }
    } else if let Node::Bin { kind, lhs, rhs } = node {
        let mut s = format!("{} [label=\"{:?}\"];\n", name, kind);
        let left = format!("{}l", name);
        let right = format!("{}r", name);
        s = s + &graph_gen(lhs, left.clone()) + &graph_gen(rhs, right.clone());
        s = s + &format!("{} -> {{ {} {} }};\n", name, left, right);
        s
    } else if let Node::Unary {
        kind: NdReturn,
        next,
    } = node
    {
        let mut s = format!("{} [label=\"return\"];\n", name);
        let nextname = name.clone() + &"n";
        s.push_str(&graph_gen(next, nextname.clone()));
        s + &format!("{} -> {}", name, nextname)
    } else {
        unimplemented!();
    }
}
