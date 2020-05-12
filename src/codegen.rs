use crate::parse::Node;
use crate::parse::NodeKind;

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
#[derive(Default)]
struct CodeGenerator {
    if_count: usize,
    loop_count: usize,
    // for_count: usize,
}
fn gen_lval(offset: &usize) {
    // println!("gen lval {:?}", &node);
    // rbpは関数の先頭アドレス
    // そこからoffset分引くと、目的の変数のアドレスを得る
    // それをpush
    // 要するに、gen(node)が結果の値をpushするのに対し、
    // gen_lvalはoffsetにあるアドレスをpushする
    // 代入する場合と値を用いる場合で、その後の扱いを変えること
    println!("  mov rax, rbp");
    println!("  sub rax, {}", offset);
    println!("  push rax\n");
}
fn load() {
    // アドレスを取り出し、値を取得してpushし直す
    println!("  pop rax");
    println!("  mov rax, [rax]");
    println!("  push rax");
}
fn store() {
    // 下から 値 | アドレス と並んでいるときに、
    // 値をそのアドレスに格納し、その値をpush
    println!("  pop rdi");
    println!("  pop rax");
    println!("  mov [rax], rdi");
    println!("  push rdi");
}
impl CodeGenerator {
    fn gen(&mut self, node: &Node) -> Result<(), CodeGenError> {
        match node {
            Node::Num { val, .. } => {
                println!("  push {}", val);
            }
            Node::Lvar { offset, .. } => {
                gen_lval(offset); // まず変数のアドレスを取得する
                load(); // そのアドレスを参照して値をpush
            }
            Node::Return { returns } => {
                self.gen(returns)?; // その値を取得し
                println!("  pop rax"); // raxに移す
                println!("  jmp .L.return");
            }
            Node::If {
                condi,
                then_,
                else_,
            } => {
                // 左辺を計算して結果をpush
                self.gen(condi)?;
                // condi結果取り出し
                println!("  pop rax");
                println!("  cmp rax, 0");
                // 結果がfalseならjump, trueならそのまま
                println!("  je .ELSE{}", self.if_count);
                if let Some(else_) = else_ {
                    // trueの場合
                    self.gen(then_)?;
                    println!("  jmp .IF{}", self.if_count);
                    println!(".ELSE{}:", self.if_count);
                    // falseの場合
                    self.gen(else_)?; // こっちはjmp不要(次の行の.L1にそのまますすむ)
                    println!(".IF{}:", self.if_count);
                } else {
                    self.gen(then_)?;
                    println!(".ELSE{}:", self.if_count);
                }
                self.if_count += 1;
            }
            Node::While { condi, then_ } => {
                println!(".BEGIN{}:", self.loop_count);
                self.gen(condi)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je .END{}", self.loop_count);
                self.gen(then_)?;
                println!("  jmp .BEGIN{}", self.loop_count);
                println!(".END{}:", self.loop_count);
                self.loop_count += 1;
            }
            Node::For {
                start,
                condi,
                end,
                loop_,
            } => {
                if let Some(start) = start {
                    self.gen(start)?;
                }
                println!(".BEGIN{}:", self.loop_count);
                if let Some(condi) = condi {
                    self.gen(condi)?;
                } else {
                    println!("  push 1");
                }
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je .END{}", self.loop_count);
                self.gen(loop_)?;
                if let Some(end) = end {
                    self.gen(end)?;
                }
                println!("  jmp .BEGIN{}", self.loop_count);
                println!(".END{}:", self.loop_count);
                self.loop_count += 1;
            }
            Node::Assign { offset, rhs, .. } => {
                gen_lval(offset);
                self.gen(rhs)?;
                store();
            }
            Node::Block { stmts } => {
                for stmt in stmts {
                    self.gen(stmt);
                    println!("  pop rax");
                }
            }
            Node::Func { name } => {
                println!("  call {}", name);
                println!("  push rax");
            }
            Node::Bin { kind, lhs, rhs } => {
                self.gen(lhs)?;
                self.gen(rhs)?;
                println!("  pop rdi");
                println!("  pop rax");
                let code = match kind {
                    NodeKind::NdAdd => "  add rax, rdi",
                    NodeKind::NdSub => "  sub rax, rdi",
                    NodeKind::NdMul => "  imul rax, rdi",
                    NodeKind::NdDiv => "  cqo\n  idiv rdi",
                    NodeKind::NdEq => "  cmp rax, rdi\n  sete al\n  movzb rax, al", // 最後のmovzbは、raxの上位56を削除して、下位8ビットにalを入れるということだろう。
                    NodeKind::NdNeq => "  cmp rax, rdi\n  setne al\n  movzb rax, al",
                    NodeKind::NdLt => "  cmp rax, rdi\n  setl al\n  movzb rax, al",
                    NodeKind::NdLe => "  cmp rax, rdi\n  setle al\n  movzb rax, al",
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
}
pub fn code_gen(nodes: Vec<Node>, varoffset: usize) -> Result<(), CodeGenError> {
    const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
    // まず現在の関数のrbpをスタックにpush(戻る場所)、次に現在のrsp(スタックポインタ)の値をrbpに格納し、rspを使用する変数分ずらす
    println!("{}", HEADER);
    println!("  push rbp"); // まず現在の関数のrbpをスタックにpush(戻る場所?)
    println!("  mov rbp, rsp"); // 次にrbpに現在のrspを入れる。rspは常にスタックの1番下を指していて、rbpもそこを指すようになる
    println!("  sub rsp, {}\n", varoffset); // rspを変数分ずらす。この時スタックは自動的にずれる。(26回pushするのと同じ?)
    let mut cg = CodeGenerator::default();
    for node in &nodes {
        cg.gen(node)?;
        println!("  pop rax\n");
    }
    println!(".L.return:");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
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
        Node::While { condi, then_ } => {
            s += &format!("{} [label=\"while\"];\n", nodename);
            s += &graph_gen(condi, Some(&nodename), 0);
            s += &graph_gen(then_, Some(&nodename), 1);
        }
        Node::For {
            start,
            condi,
            end,
            loop_,
        } => {
            s += &format!("{} [label=\"for\"];\n", nodename);
            if let Some(n) = start {
                s += &graph_gen(n, Some(&nodename), 0);
            }
            if let Some(n) = condi {
                s += &graph_gen(n, Some(&nodename), 1);
            }
            if let Some(n) = end {
                s += &graph_gen(n, Some(&nodename), 2);
            }
            s += &graph_gen(loop_, Some(&nodename), 3);
        }
        Node::Block { stmts } => {
            s += &format!("{} [label=\"block\"];\n", nodename);
            for (i, stmt) in stmts.iter().enumerate() {
                s += &graph_gen(stmt, Some(&nodename), i);
            }
        }
        _ => {
            unimplemented!();
        }
    }
    s
}
