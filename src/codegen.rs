use crate::parse::Node;
use crate::parse::NodeKind::*;

#[derive(Debug, Default)]
pub struct CodeGenError {
    pub msg: String,
}
impl CodeGenError {
    fn new(node: &Node) -> Self {
        Self {
            msg: format!("{:?}", node),
        }
    }
}

fn gen_lval(offset: &usize) {
    // println!("gen lval {:?}", &node);
    // rbpは関数の先頭アドレス
    // そこからoffset分引くと、目的の変数のアドレスを得る
    // それをpush
    // 要するに、gen(node)が結果の値をpushするのに対し、
    // gen_lvalは結果のアドレスをpushする
    // 結果が変数でないときは、どうなるん?(node.offsetがバグる)
    println!("  mov rax, rbp");
    println!("  sub rax, {}", offset);
    println!("  push rax\n");
}
fn gen(node: &Node) -> Result<(), CodeGenError> {
    match node {
        Node::Num { val, .. } => {
            println!("  push {}", val);
        }
        Node::Return { returns } => {
            // nextはlvarが期待されている
            gen(returns)?; // その値を取得す?る
            println!("  pop rax");
            println!("  mov rsp, rbp"); // スタックをrbpまで戻し
            println!("  pop rbp"); // 以前のrbpがそこにあるので回収し、
            println!("  ret"); // 関数を抜ける
        }
        Node::Lvar { offset, .. } => {
            // まず変数のアドレスを取得する
            gen_lval(offset);
            // そのアドレスを参照してpush
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
        }
        Node::If {
            condi,
            then_,
            else_,
        } => {
            // 左辺を計算して結果をpush
            gen(condi)?;
            // condi結果取り出し
            println!("  pop rax");
            println!("  cmp rax, 0");
            // 結果がfalseならjump, trueならそのまま
            println!("  je .L0");
            if let Some(n) = else_ {
                // trueの場合
                gen(then_)?;
                println!("  jmp .L1\n");
                println!(".L0:");
                // falseの場合
                gen(n)?; // こっちはjmp不要(次の行の.L1にそのまますすむ)
                println!(".L1:");
            } else {
                gen(then_)?;
                println!(".L0");
            }
        }
        Node::Assign { offset, rhs, .. } => {
            gen_lval(offset);
            gen(rhs)?;
            // rdi->変数アドレス, rax->右辺の結果
            println!("  pop rdi");
            println!("  pop rax");
            // 右辺に代入して、ついでにその値をpush(cの代入式は、代入した値を持つ)
            println!("  mov [rax], rdi");
            println!("  push rdi");
        }
        Node::Bin { kind, lhs, rhs } => {
            gen(lhs)?;
            gen(rhs)?;
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
        _ => Err(CodeGenError::new(node))?,
    }
    println!("");
    Ok(())
}
pub fn code_gen(nodes: Vec<Node>, varoffset: usize) -> Result<(), CodeGenError> {
    const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
    // まず現在の関数のrbpをスタックにpush(戻る場所)、次に現在のrsp(スタックポインタ)の値をrbpに格納し、rspを使用する変数分ずらす
    println!("{}", HEADER);
    println!("  push rbp"); // まず現在の関数のrbpをスタックにpush(戻る場所?)
    println!("  mov rbp, rsp"); // 次にrbpに現在のrspを入れる。rspは常にスタックの1番下を指していて、rbpもそこを指すようになる
    println!("  sub rsp, {}\n", varoffset); // rspを変数分ずらす。この時スタックは自動的にずれる。(26回pushするのと同じ?)
    for node in &nodes {
        gen(node)?;
        println!("  pop rax\n");
    }
    println!("  ret");
    Ok(())
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
        f.write(graph_gen(node, None, i).as_bytes())?;
    }
    f.write(b"}\n")?;
    Ok(())
}
pub fn graph_gen(node: &Node, parent: Option<&String>, number: usize) -> String {
    // 親が指定されているとき、そこから自分への辺を引く
    let mut s = String::new();
    let nodename = if let Some(pname) = parent {
        format!("{}{}", pname, number)
    } else {
        format!("{}", number)
    };
    if let Some(pname) = parent {
        s += &format!("{} -> {}\n", pname, nodename);
    }
    match node {
        Node::Num { val } => s += &format!("{} [label=\"num {}\"];\n", nodename, val),
        Node::Lvar { name, offset } => {
            s += &format!("{} [label=\"var {} ofs:{}\"];\n", nodename, name, offset)
        }
        Node::Bin { kind, lhs, rhs } => {
            s += &format!("{} [label=\"{:?}\"];\n", nodename, kind);
            s += &graph_gen(lhs, Some(&nodename), 0);
            s += &graph_gen(rhs, Some(&nodename), 1);
        }
        Node::Assign { name, rhs, .. } => {
            s += &format!("{} [label=\"assign {}\"];\n", nodename, name);
            s += &graph_gen(rhs, Some(&nodename), 0);
        }
        Node::Return { returns } => {
            s += &format!("{} [label=\"return\"];\n", nodename);
            s += &graph_gen(returns, Some(&nodename), 0);
        }
        Node::If {
            condi,
            then_,
            else_,
        } => {
            s += &format!("{} [label=\"if\"];\n", nodename);
            s += &graph_gen(condi, Some(&nodename), 0);
            s += &graph_gen(then_, Some(&nodename), 1);
            if let Some(n) = else_ {
                s += &graph_gen(n, Some(&nodename), 2);
            }
        }
        _ => {
            unimplemented!();
        }
    }
    s
}
