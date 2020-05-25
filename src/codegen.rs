use crate::parse::{Function, Node, NodeKind, Var};
use std::rc::Rc;

// 関数呼び出しのレジスタ 参考 https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions
const ARGREG: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const ARGLEN: usize = 6;
const RESERVED_REGISTER_STACK_SIZE: usize = 32;

#[derive(Debug, Default)]
pub struct CodeGenError {
    pub msg: String,
}
// impl CodeGenError {
//     fn new(node: &Node) -> Self {
//         Self {
//             msg: format!("{:?}", node),
//         }
//     }
// }
#[derive(Default)]
struct CodeGenerator {
    label_count: usize,
    func_name: String,
    var_offsets: Vec<usize>,
    func_stack_size: usize,
}
fn gen_addr(offset: usize) {
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
    fn set_var_offset(&mut self, lvars: &Vec<Rc<Var>>) {
        // lvarsもvar_offsetもvar.idもあくまで出現順である
        let var_num = lvars.len();
        self.var_offsets = vec![0; var_num];
        let mut offset = RESERVED_REGISTER_STACK_SIZE;
        for i in (0..var_num).rev() {
            offset += lvars[i].ty.size();
            self.var_offsets[i] = offset;
        }
        // println!("{:?}", lvars);
        // println!("{:?}", self.var_offsets);
        // 以下、スタックサイズを16の倍数に揃える
        self.func_stack_size = *self
            .var_offsets
            .first()
            .unwrap_or(&RESERVED_REGISTER_STACK_SIZE)
            - 1;
        self.func_stack_size -= self.func_stack_size % 16;
        self.func_stack_size += 16;
    }
    fn gen(&mut self, node: &Node) -> Result<(), CodeGenError> {
        match node {
            Node::Num { val, .. } => {
                println!("  push {}", val);
            }
            Node::Lvar { var } => {
                gen_addr(self.var_offsets[var.id]); // まず変数のアドレスを取得する
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
            Node::Addr { node } => {
                if let Node::Lvar { var } = &**node {
                    gen_addr(self.var_offsets[var.id]);
                } else {
                    return Err(CodeGenError {
                        msg: "invalid address expression!".to_owned(),
                    });
                }
            }
            Node::Deref { node } => {
                self.gen(node)?;
                load();
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
            Node::Assign { lvar, rhs, .. } => {
                println!("  #assign");
                match &**lvar {
                    Node::Lvar { var } => {
                        gen_addr(self.var_offsets[var.id]);
                    }
                    Node::Deref { node } => {
                        self.gen(node)?;
                    }
                    _ => {
                        return Err(CodeGenError {
                            msg: format!("lhs of assignment must be var or deref!"),
                        });
                    }
                }
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
                for i in (0..args.len()).rev() {
                    println!("  pop {}", ARGREG[i])
                }
                println!("  call {}", name);
                println!("  push rax"); // 関数終了時にreturnの値がraxに入っている。
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
        cg.set_var_offset(&func.locals);
        println!(".globl {}", cg.func_name);
        println!("{}:", cg.func_name);

        println!("  push rbp"); // まず現在の関数のrbpをスタックにpush(戻る場所?)
        println!("  mov rbp, rsp"); // 次にrbpに現在のrspを入れる。rspは常にスタックの1番下を指していて、rbpもそこを指すようになる
        println!("  sub rsp, {}", cg.func_stack_size);

        println!("  mov [rbp-8], r12");
        println!("  mov [rbp-16], r13");
        println!("  mov [rbp-24], r14");
        println!("  mov [rbp-32], r15");
        // 以下レジスタに保存されていた引数がローカル変数に格納される
        // ただし、変数のオフセットは最初に出現したものが一番下になる
        // 例えば offsetが全体で64で、関数がint f(int a, int b, ...)となっていたら
        //   mov [rbp-64], rdi
        //   mov [rbp-56], rsi
        // となる
        // lvarsもvar_offsetもvar.idもあくまで出現順である
        for i in 0..func.params.len() {
            println!("  mov [rbp-{}], {}", cg.var_offsets[i], ARGREG[i])
        }
        for node in &func.body {
            cg.gen(node)?;
        }
        println!(".L.return.{}:", cg.func_name);
        println!("  mov [rbp-8], r12");
        println!("  mov [rbp-16], r13");
        println!("  mov [rbp-24], r14");
        println!("  mov [rbp-32], r15");
        println!("  mov rsp, rbp");
        println!("  pop rbp"); // 呼び出し時のrbpをrbpに回収
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
        Node::Lvar { var } => s += &format!("{} [label=\"{:?}\"];\n", nodename, var),
        Node::Bin { kind, lhs, rhs } => {
            s += &format!("{} [label=\"{:?}\"];\n", nodename, kind);
            s += &graph_gen(lhs, Some(&nodename), 0);
            s += &graph_gen(rhs, Some(&nodename), 1);
        }
        Node::Assign { lvar, rhs } => {
            s += &format!("{} [label=\"assign\"];\n", nodename);
            s += &graph_gen(lvar, Some(&nodename), 0);
            s += &graph_gen(rhs, Some(&nodename), 1);
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
