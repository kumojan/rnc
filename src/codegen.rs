use crate::parse::{Function, Node, NodeKind};

// const ARGREG: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const ARGREG: [&'static str; 6] = ["r9", "r8", "rcx", "rdx", "rsi", "rdi"]; // 第6引数から順に第1引数まで
const ARGLEN: usize = 6;

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
    label_count: usize,
    func_name: String,
}
fn gen_addr(offset: &usize) {
    // println!("gen lval {:?}", &node);
    // rbpは関数の先頭アドレス
    // そこからoffset分引くと、目的の変数のアドレスを得る
    // それをpush
    // 普通movとかは
    // 要するに、gen(node)が結果の値をpushするのに対し、
    // gen_addrはoffsetにあるアドレスをpushする
    // 代入する場合と値を用いる場合で、その後の扱いを変えること
    println!("  lea rax, [rbp-{}]", offset);
    println!("  push rax");
}
fn load() {
    // アドレスを取り出し、値を取得してpushし直す
    println!("  pop rax  #load");
    println!("  mov rax, [rax]");
    println!("  push rax");
}
fn store() {
    // 下から 値 | アドレス と並んでいるときに、
    // 値をそのアドレスに格納し、その値をpush
    println!("  pop rdi  #store");
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
                gen_addr(offset); // まず変数のアドレスを取得する
                load(); // そのアドレスを参照して値をpush
            }
            Node::Return { returns } => {
                self.gen(returns)?; // その値を取得し
                println!("  pop rax"); // raxに移す
                println!("  jmp .L.return.{}", self.func_name);
            }
            Node::ExprStmt { expr } => {
                self.gen(expr)?;
                println!("  pop rax"); // 最後に評価した値を捨てる
            }
            Node::Addr { node } => {}
            Node::Deref { node } => {}
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
                println!("  je .L.else.{}", self.label_count);
                if let Some(else_) = else_ {
                    // trueの場合
                    self.gen(then_)?;
                    println!("  jmp .L.end.{}", self.label_count);
                    println!(".L.else.{}:", self.label_count);
                    // falseの場合
                    self.gen(else_)?; // こっちはjmp不要(次の行の.L1にそのまますすむ)
                    println!(".L.end.{}:", self.label_count);
                } else {
                    self.gen(then_)?;
                    println!(".L.else.{}:", self.label_count);
                }
                self.label_count += 1;
            }
            Node::While { condi, then_ } => {
                println!(".L.begin.{}:", self.label_count);
                self.gen(condi)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je .L.end.{}", self.label_count);
                self.gen(then_)?;
                println!("  jmp .L.begin.{}", self.label_count);
                println!(".L.end.{}:", self.label_count);
                self.label_count += 1;
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
                println!(".L.begin.{}:", self.label_count);
                if let Some(condi) = condi {
                    self.gen(condi)?;
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .L.end.{}", self.label_count);
                }
                self.gen(loop_)?;
                if let Some(end) = end {
                    self.gen(end)?;
                }
                println!("  jmp .L.begin.{}", self.label_count);
                println!(".L.end.{}:", self.label_count);
                self.label_count += 1;
            }
            Node::Assign { offset, rhs, .. } => {
                println!("  #assign");
                gen_addr(offset);
                self.gen(rhs)?;
                store();
            }
            Node::Block { stmts } => {
                for stmt in &**stmts {
                    self.gen(stmt)?;
                }
            }
            Node::FunCall { name, args } => {
                if args.len() > ARGLEN {
                    return Err(CodeGenError {
                        msg: format!("too many arguments for func {} (must be less than 7)", name),
                    });
                }
                for arg in &**args {
                    self.gen(arg)?;
                }
                for reg in &ARGREG[ARGLEN - args.len()..] {
                    println!("  pop {}", reg);
                }
                println!("  mov rax, rsp");
                println!("  and rax, 15"); // rspのが16の倍数かどうか見ている
                println!("  jnz .L.call.{}", self.label_count); // 16の倍数でないとき?

                // 16の倍数なとき?
                println!("  mov rax, 0");
                println!("  call {}", name);
                println!("  jmp .L.end.{}", self.label_count);

                // 16の倍数でないとき?
                println!(".L.call.{}:", self.label_count);
                println!("  sub rsp, 8");
                println!("  mov rax, 0");
                println!("  call {}", name);
                println!("  add rsp, 8");

                // 合流
                println!(".L.end.{}:", self.label_count);
                println!("  push rax");
                self.label_count += 1;
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
                };
                println!("{}", code);
                println!("  push rax")
            }
        }
        Ok(())
    }
}
pub fn code_gen(program: Vec<Function>) -> Result<(), CodeGenError> {
    // まず現在の関数のrbpをスタックにpush(戻る場所)、次に現在のrsp(スタックポインタ)の値をrbpに格納し、rspを使用する変数分ずらす

    let mut cg = CodeGenerator::default();
    println!(".intel_syntax noprefix");
    for func in program {
        cg.func_name = func.name;
        println!(".globl {}", cg.func_name);
        println!("{}:", cg.func_name);

        println!("  push rbp"); // まず現在の関数のrbpをスタックにpush(戻る場所?)
        println!("  mov rbp, rsp"); // 次にrbpに現在のrspを入れる。rspは常にスタックの1番下を指していて、rbpもそこを指すようになる
        println!("  sub rsp, {}", func.stack_size);

        for node in &func.body {
            cg.gen(node)?;
        }
        println!(".L.return.{}:", cg.func_name);
        println!("  mov rsp, rbp");
        println!("  pop rbp");
        println!("  ret");
    }
    Ok(())
}

use std::fs;
use std::io;
use std::io::{BufWriter, Write};
pub fn print_graph(program: &Vec<Function>) -> io::Result<()> {
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
    for func in program {
        for (i, node) in func.body.iter().enumerate() {
            f.write(format!("{} [label=\"func {}\"]\n", func.name, func.name).as_bytes())?;
            f.write(graph_gen(node, Some(&func.name), i).as_bytes())?;
        }
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
        Node::ExprStmt { expr } => {
            s += &format!("{} [label=\"statement\"];\n", nodename);
            s += &graph_gen(expr, Some(&nodename), 0);
        }
        Node::Addr { node } => {
            s += &format!("{} [label=\"addr\"];\n", nodename);
            s += &graph_gen(node, Some(&nodename), 0);
        }
        Node::Deref { node } => {
            s += &format!("{} [label=\"deref\"];\n", nodename);
            s += &graph_gen(node, Some(&nodename), 0);
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
        Node::FunCall { name, args } => {
            s += &format!("{} [label=\"func call {}\"];\n", nodename, name);
            for (i, arg) in args.iter().enumerate() {
                s += &graph_gen(arg, Some(&nodename), i);
            }
        }
    }
    s
}
