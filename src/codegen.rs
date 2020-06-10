use crate::parse::{BinOp, Function, Node, NodeKind, Var};
use crate::r#type::Type;
use crate::util::*;
use std::rc::Rc;

// 関数呼び出しのレジスタ 参考 https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions
const ARGREG64: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const ARGREG32: [&'static str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
const ARGREG16: [&'static str; 6] = ["di", "si", "dx", "cx", "r8w", "r9w"];
const ARGREG8: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const ARGLEN: usize = 6;
const RESERVED_REGISTER_STACK_SIZE: usize = 32;

#[derive(Debug, Default)]
pub struct CodeGenError {
    pub msg: String,
    pub pos: usize,
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
    pos: usize,
}
fn load(ty: &Type) {
    if ty.is_array() {
        return;
    }
    if let Type::TyStruct { .. } = ty {
        return; // 構造体のloadもアドレス
    }
    // アドレスを取り出し、値を取得してpushし直す
    println!("  pop rax");
    match ty.size() {
        1 => println!("  movsx rax, byte ptr [rax]\n"),
        2 => println!("  movsx rax, word ptr [rax]\n"),
        4 => println!("  movsx rax, dword ptr [rax]\n"),
        _ => println!("  mov rax, [rax]"),
    }
    println!("  push rax");
}
fn store(ty: &Type) {
    // 下から 値 | アドレス と並んでいるときに、
    // 値をそのアドレスに格納し、その値をpush
    println!("  pop rdi  # store {:?}", ty);
    println!("  pop rax");
    if let Type::TyStruct { size, .. } = ty {
        // この時rax, rdiはそれぞれ左辺、右辺のアドレス
        for i in 0..*size {
            println!("  mov dl, [rdi+{}]", i); // 右辺のアドレスから1バイト読み、
            println!("  mov [rax+{}], dl", i); // 左辺のアドレスの位置に1バイト書き込む。dlは汎用8ビットレジスタ
        }
    } else {
        // この時rax, rdiはそれぞれ左辺のアドレスと、右辺の値
        let regname = match ty.size() {
            1 => "dil",
            2 => "di",
            4 => "edi",
            _ => "rdi",
        };
        println!("  mov [rax], {}", regname);
    }
    println!("  push rdi");
}
fn cast(ty: &Type) {
    if ty == &Type::TyVoid || ty.size() == 8 {
        return;
    }
    println!("  pop rax");
    match ty.size() {
        1 => println!("  movsx rax, al"),
        2 => println!("  movsx rax, ax"),
        4 => println!("  movsx rax, eax"),
        _ => unimplemented!(),
    }
    println!("  push rax");
}

impl CodeGenerator {
    fn gen_var_addr(&self, v: &Rc<Var>) {
        // println!("gen lval {:?}", &node);
        // rbpは関数の先頭アドレス
        // そこからoffset分引くと、目的の変数のアドレスを得る
        // それをpush
        // 普通movとかは
        // 要するに、gen(node)が結果の値をpushするのに対し、
        // gen_addrはoffsetにあるアドレスをpushする
        // 代入する場合と値を用いる場合で、その後の扱いを変えること
        if v.is_local {
            println!(
                "  lea rax, [rbp-{}]  # load {}",
                self.var_offsets[v.id], v.name
            );
            println!("  push rax");
        } else {
            println!("  push offset {}", v.name);
        }
    }
    fn gen_addr(&mut self, node: &Node) -> Result<(), CodeGenError> {
        if let Some(tok) = &node.tok {
            self.pos = tok.pos;
            println!(".loc 1 {}", tok.line_no);
        }
        match &node.kind {
            NodeKind::Var { var } => self.gen_var_addr(var),
            NodeKind::Deref { node } => self.gen_expr(node)?,
            NodeKind::Member { obj, mem } => {
                self.gen_addr(obj)?;
                // 構造体の中でのoffsetだけずらす
                println!(
                    "  pop rax  # load {}\n  add rax, {}\n  push rax",
                    mem.name, mem.offset
                );
            }
            NodeKind::Comma { lhs, rhs } => {
                self.gen_expr(lhs)?;
                println!("  pop rax");
                self.gen_addr(rhs)?;
            }
            _ => Err(CodeGenError {
                pos: self.pos,
                msg: format!("not a left value! {:?}", node.kind),
            })?,
        };
        Ok(())
    }
    fn set_var_offset(&mut self, lvars: &Vec<Rc<Var>>) {
        // lvarsもvar_offsetもvar.idもあくまで出現順である
        let var_num = lvars.len();
        self.var_offsets = vec![0; var_num];
        let mut offset = RESERVED_REGISTER_STACK_SIZE;
        for i in (0..var_num).rev() {
            offset = align_to(offset, lvars[i].ty.align());
            offset += lvars[i].ty.size();
            self.var_offsets[i] = offset;
        }
        let offset = *self
            .var_offsets
            .first()
            .unwrap_or(&RESERVED_REGISTER_STACK_SIZE);
        self.func_stack_size = align_to(offset, 16);
    }
    fn gen_expr(&mut self, node: &Node) -> Result<(), CodeGenError> {
        if let Some(tok) = &node.tok {
            self.pos = tok.pos;
            println!(".loc 1 {}", tok.line_no);
        }
        match &node.kind {
            NodeKind::Num { val, .. } => {
                // println!("  push {}", val);
                println!("  mov rax, {}", val); // raxに一旦入れると、アセンブリに文句を言われず、cのintやlongと相性が良さそう。(あまり理解していない)
                println!("  push rax");
            }
            NodeKind::Var { var } => {
                self.gen_addr(&node)?; // まず変数のアドレスを取得する
                load(&var.ty); // 配列型の場合は、値を取り出さず、アドレスをそのまま使う
            }
            NodeKind::Literal { id, .. } => {
                println!("  push offset .L.data.{}", id);
            }
            NodeKind::Cast(expr) => {
                self.gen_expr(expr)?;
                if let Some(ty) = &node.ty {
                    cast(ty);
                } else {
                    unreachable!();
                }
            }
            NodeKind::Addr { node } => self.gen_addr(node)?,
            NodeKind::Deref { node } => {
                // **x(2段回)だと、gen(x); load(); load();
                // となる。つまりxの結果(xが変数ならば、その値)を取得し、
                // それをアドレスとして値を取得、
                // さらにそれをアドレスとして値を取得、となる
                self.gen_expr(node)?;
                // 元々の型(このnodeをderefした結果)がarrayなら、やはりアドレス(評価結果)をそのまま使う
                // それ以外なら値を取得する
                // TODO: 型エラーの処理
                load(&node.get_type().get_base().unwrap());
            }
            NodeKind::Assign { lvar, rhs, .. } => {
                self.gen_addr(lvar)?;
                self.gen_expr(rhs)?;
                store(&lvar.get_type()); // どの型に代入するかによってコードが異なる
            }
            NodeKind::Comma { lhs, rhs } => {
                self.gen_expr(lhs)?;
                println!("  pop rax");
                self.gen_expr(rhs)?;
            }
            NodeKind::FunCall { name, args } => {
                if args.len() > ARGLEN {
                    return Err(CodeGenError {
                        pos: self.pos,
                        msg: format!("too many arguments for func {} (must be less than 7)", name),
                    });
                }
                for arg in &**args {
                    self.gen_expr(arg)?; // 生成されたexprが8バイトずつ格納されている (本来のサイズにかかわらず)
                }
                for i in (0..args.len()).rev() {
                    println!("  pop {}", ARGREG64[i])
                }
                println!("  call {}", name);
                println!("  push rax"); // 関数終了時にreturnの値がraxに入っている。
            }
            NodeKind::StmtExpr { stmts, expr } => {
                self.gen_stmt(stmts)?;
                self.gen_expr(expr)?;
            }
            NodeKind::Member { .. } => {
                self.gen_addr(&node)?;
                load(&node.get_type())
            }
            NodeKind::Bin { kind, lhs, rhs } => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                println!("  pop rdi");
                println!("  pop rax");
                let code = match kind {
                    BinOp::Add => "  add rax, rdi",
                    BinOp::Sub => "  sub rax, rdi",
                    BinOp::Mul => "  imul rax, rdi",
                    BinOp::Div => "  cqo\n  idiv rdi",
                    BinOp::_Eq => "  cmp rax, rdi\n  sete al\n  movzb rax, al", // 最後のmovzbは、raxの上位56を削除して、下位8ビットにalを入れるということだろう。
                    BinOp::Neq => "  cmp rax, rdi\n  setne al\n  movzb rax, al",
                    BinOp::Lt => "  cmp rax, rdi\n  setl al\n  movzb rax, al",
                    BinOp::Le => "  cmp rax, rdi\n  setle al\n  movzb rax, al",
                };
                println!("{}", code);
                println!("  push rax")
            }
            _ => {
                return Err(CodeGenError {
                    pos: self.pos,
                    msg: "invalid expression!".to_owned(),
                })
            }
        }
        Ok(())
    }
    fn gen_stmt(&mut self, node: &Node) -> Result<(), CodeGenError> {
        if let Some(tok) = &node.tok {
            self.pos = tok.pos;
            println!(".loc 1 {}", tok.line_no);
        }
        match &node.kind {
            NodeKind::Return { returns } => {
                self.gen_expr(returns)?; // その値を取得し
                println!("  pop rax"); // raxに移す
                println!("  jmp .L.return.{}", self.func_name);
            }
            NodeKind::Block { stmts } => {
                for stmt in &**stmts {
                    self.gen_stmt(stmt)?;
                }
            }
            NodeKind::ExprStmt { expr } => {
                self.gen_expr(expr)?;
                println!("  pop rax"); // 最後に評価した値を捨てる
            }
            NodeKind::If {
                condi,
                then_,
                else_,
            } => {
                let label = self.label_count;
                self.label_count += 1;
                // 左辺を計算して結果をpush
                self.gen_expr(condi)?;
                // condi結果取り出し
                println!("  pop rax");
                println!("  cmp rax, 0");
                // 結果がfalseならjump, trueならそのまま
                println!("  je .L.else.{}", label);
                if let Some(else_) = else_ {
                    // trueの場合
                    self.gen_stmt(then_)?;
                    println!("  jmp .L.end.{}", label);
                    println!(".L.else.{}:", label);
                    // falseの場合
                    self.gen_stmt(else_)?; // こっちはjmp不要(次の行の.L1にそのまますすむ)
                    println!(".L.end.{}:", label);
                } else {
                    self.gen_stmt(then_)?;
                    println!(".L.else.{}:", label);
                }
            }
            NodeKind::For {
                start,
                condi,
                end,
                loop_,
            } => {
                let label = self.label_count;
                self.label_count += 1;
                if let Some(start) = start {
                    self.gen_stmt(start)?;
                }
                println!(".L.begin.{}:", label);
                if let Some(condi) = condi {
                    self.gen_expr(condi)?;
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .L.end.{}", label);
                }
                self.gen_stmt(loop_)?;
                if let Some(end) = end {
                    self.gen_stmt(end)?;
                }
                println!("  jmp .L.begin.{}", label);
                println!(".L.end.{}:", label);
            }
            _ => {
                return Err(CodeGenError {
                    pos: self.pos,
                    msg: "invalid expression!".to_owned(),
                })
            }
        }
        Ok(())
    }
}
pub fn code_gen(
    program: Vec<Function>,
    globals: Vec<Rc<Var>>,
    string_literals: Vec<Vec<u8>>,
) -> Result<(), CodeGenError> {
    let mut cg = CodeGenerator::default();
    println!(".intel_syntax noprefix");

    println!(".data");
    for v in &globals {
        println!("{}:", v.name);
        println!("  .zero {}", v.ty.size());
    }
    for (i, s) in string_literals.iter().enumerate() {
        println!(".L.data.{}:", i);
        for c in s {
            println!("  .byte {}", c);
        }
        println!("  .byte 0");
    }
    println!(".text");
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
            let reg = match func.params[0].ty.size() {
                1 => ARGREG8[i],
                2 => ARGREG16[i],
                4 => ARGREG32[i],
                _ => ARGREG64[i],
            };
            println!("  mov [rbp-{}], {}", cg.var_offsets[i], reg)
        }
        for node in &func.body {
            cg.gen_stmt(node)?;
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
use std::io::{BufWriter, Write};
pub fn print_graph(program: &Vec<Function>) {
    let mut f = BufWriter::new(fs::File::create("graph.dot").unwrap());
    f.write(
        b"digraph {
node [
style = \"filled\",
fontsize = 16,
fillcolor = \"green\",
];
",
    )
    .unwrap();
    for func in program {
        for (i, node) in func.body.iter().enumerate() {
            f.write(format!("{} [label=\"func {}\"]\n", func.name, func.name).as_bytes())
                .unwrap();
            f.write(graph_gen(node, &func.name, i, None).as_bytes())
                .unwrap();
        }
    }

    f.write(b"}\n").unwrap();
}
pub fn graph_gen(node: &Node, parent: &String, number: usize, arrow: Option<&str>) -> String {
    if let NodeKind::ExprStmt { expr } = &node.kind {
        return graph_gen(expr, &parent, number, Some("stmt"));
    }
    // 親が指定されているとき、そこから自分への辺を引く
    let mut s = String::new();
    let nodename = format!("{}{}", parent, number);
    if let Some(arrow) = arrow {
        s += &format!("{} -> {} [label=\"{}\"]\n", parent, nodename, arrow);
    } else {
        s += &format!("{} -> {}\n", parent, nodename);
    }
    match &node.kind {
        NodeKind::Num { val } => s += &format!("{} [label=\"num {}\"];\n", nodename, val),
        NodeKind::Cast(expr) => {
            s += &format!("{} [label=\"cast {:?}\"];\n", nodename, node.ty);
            s += &graph_gen(expr, &nodename, 0, None);
        }
        NodeKind::Var { var } => s += &format!("{} [label=\"{:?}\"];\n", nodename, var),
        NodeKind::Literal { id, .. } => s += &format!("{} [label=\"literal {}\"];\n", nodename, id),
        NodeKind::Bin { kind, lhs, rhs } => {
            s += &format!("{} [label=\"{:?}\"];\n", nodename, kind);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::Assign { lvar, rhs } => {
            s += &format!("{} [label=\"assign\"];\n", nodename);
            s += &graph_gen(lvar, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::Comma { lhs, rhs } => {
            s += &format!("{} [label=\"comma\"];\n", nodename);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::Return { returns } => {
            s += &format!("{} [label=\"return\"];\n", nodename);
            s += &graph_gen(returns, &nodename, number, None);
        }
        NodeKind::ExprStmt { .. } => panic!(),
        NodeKind::Addr { node } => {
            s += &format!("{} [label=\"addr\"];\n", nodename);
            s += &graph_gen(node, &nodename, 0, None);
        }
        NodeKind::Deref { node } => {
            s += &format!("{} [label=\"deref\"];\n", nodename);
            s += &graph_gen(node, &nodename, 0, None);
        }
        NodeKind::If {
            condi,
            then_,
            else_,
        } => {
            s += &format!("{} [label=\"if\"];\n", nodename);
            s += &graph_gen(condi, &nodename, 0, Some("condi"));
            s += &graph_gen(then_, &nodename, 1, Some("then"));
            if let Some(n) = else_ {
                s += &graph_gen(n, &nodename, 2, Some("else"));
            }
        }
        NodeKind::For {
            start,
            condi,
            end,
            loop_,
        } => {
            s += &format!("{} [label=\"for\"];\n", nodename);
            if let Some(n) = start {
                s += &graph_gen(n, &nodename, 0, Some("init"));
            }
            if let Some(n) = condi {
                s += &graph_gen(n, &nodename, 1, Some("condi"));
            }
            if let Some(n) = end {
                s += &graph_gen(n, &nodename, 2, Some("end"));
            }
            s += &graph_gen(loop_, &nodename, 3, Some("loop"));
        }
        NodeKind::Block { stmts } => {
            s += &format!("{} [label=\"block\"];\n", nodename);
            for (i, stmt) in stmts.iter().enumerate() {
                s += &graph_gen(stmt, &nodename, i, None);
            }
        }
        NodeKind::FunCall { name, args } => {
            s += &format!("{} [label=\"func call {}\"];\n", nodename, name);
            for (i, arg) in args.iter().enumerate() {
                s += &graph_gen(arg, &nodename, i, Some("args"));
            }
        }
        NodeKind::StmtExpr { stmts, expr } => {
            s += &graph_gen(stmts, &nodename, 0, Some("statements"));
            s += &graph_gen(expr, &nodename, 0, Some("return"));
        }
        NodeKind::Member { obj, .. } => {
            s += &graph_gen(obj, &nodename, 0, None);
        }
    }
    s
}
