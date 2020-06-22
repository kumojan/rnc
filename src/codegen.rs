use crate::parse::node::{BinOp, Data, Function, Node, NodeKind, Var};
use crate::r#type::Type;
use crate::tokenize::{CString, Token};
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
    tklist: Vec<Token>,
    break_label: Vec<usize>,
    continue_label: Vec<usize>,
    case_label: Vec<usize>,
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
        1 => println!("  movsx rax, byte ptr [rax]"),
        2 => println!("  movsx rax, word ptr [rax]"),
        4 => println!("  movsx rax, dword ptr [rax]"),
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
        // 配列をポインタにキャストするときは何もしない
        return;
    }
    if ty == &Type::TyBool {
        println!("  pop rax");
        println!("  cmp rax, 0");
        println!("  setne al");
        println!("  movzx rax, al");
        println!("  push rax");
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
    fn new_label(&mut self) -> usize {
        let label = self.label_count;
        self.label_count += 1;
        label
    }
    fn update_pos(&mut self, tok_no: usize) {
        let cur_tok = &self.tklist[tok_no];
        self.pos = cur_tok.pos;
        println!(".loc 1 {}", cur_tok.line_no);
    }
    fn gen_var_addr(&self, var: &Rc<Var>) {
        // println!("gen lval {:?}", &node);
        // rbpは関数の先頭アドレス
        // そこからoffset分引くと、目的の変数のアドレスを得る
        // それをpush
        // 普通movとかは
        // 要するに、gen(node)が結果の値をpushするのに対し、
        // gen_addrはoffsetにあるアドレスをpushする
        // 代入する場合と値を用いる場合で、その後の扱いを変えること
        if var.is_local && !var.is_static {
            println!(
                "  lea rax, [rbp-{}]  # load {}",
                self.var_offsets[var.id], var.name
            );
            println!("  push rax");
        } else {
            println!("  push offset {}", var.global_name());
        }
    }
    fn gen_addr(&mut self, node: &Node) -> Result<(), CodeGenError> {
        self.update_pos(node.tok_no);
        match &node.kind {
            NodeKind::Var(var) => self.gen_var_addr(var),
            NodeKind::Deref(node) => self.gen_expr(node)?,
            NodeKind::Member { obj, mem } => {
                self.gen_addr(obj)?;
                // 構造体の中でのoffsetだけずらす
                println!(
                    "  pop rax  # load {}\n  add rax, {}\n  push rax",
                    mem.name, mem.offset
                );
            }
            NodeKind::Comma(lhs, rhs) => {
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
        self.update_pos(node.tok_no);
        match &node.kind {
            NodeKind::Num(val) => {
                // println!("  push {}", val);
                println!("  mov rax, {}", val); // raxに一旦入れると、アセンブリに文句を言われず、cのintやlongと相性が良さそう。(あまり理解していない)
                println!("  push rax");
            }
            NodeKind::INum(val) => {
                // println!("  push {}", val);
                println!("  mov rax, {}", val); // raxに一旦入れると、アセンブリに文句を言われず、cのintやlongと相性が良さそう。(あまり理解していない)
                println!("  push rax");
            }
            NodeKind::Var(var) => {
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
            NodeKind::Addr(node) => self.gen_addr(node)?,
            NodeKind::Deref(node) => {
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
            NodeKind::BitNot(node) => {
                self.gen_expr(node)?;
                println!("  pop rax\n  not rax\n  push rax");
            }
            NodeKind::Assign(lhs, rhs) => {
                self.gen_addr(lhs)?;
                self.gen_expr(rhs)?;
                store(&lhs.get_type()); // どの型に代入するかによってコードが異なる
            }
            NodeKind::LogAnd(lhs, rhs) => {
                let label = self.new_label();
                self.gen_expr(lhs)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je  .L.false.{}", label);
                self.gen_expr(rhs)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je  .L.false.{}", label);
                println!("  mov rax, 1");
                println!("  jmp .L.end.{}", label);
                println!(".L.false.{}:", label);
                println!("  mov rax, 0");
                println!(".L.end.{}:", label);
                println!("  push rax");
            }
            NodeKind::LogOr(lhs, rhs) => {
                let label = self.new_label();
                self.gen_expr(lhs)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  jne  .L.true.{}", label);
                self.gen_expr(rhs)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  jne  .L.true.{}", label);
                println!("  mov rax, 0");
                println!("  jmp .L.end.{}", label);
                println!(".L.true.{}:", label);
                println!("  mov rax, 1");
                println!(".L.end.{}:", label);
                println!("  push rax");
            }
            NodeKind::Comma(lhs, rhs) => {
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
            NodeKind::Conditional {
                condi,
                then_,
                else_,
            } => {
                self.gen_expr(condi)?;
                let label = self.new_label();
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  je  .L.else.{}", label);
                self.gen_expr(then_)?;
                println!("  jmp .L.end.{}", label);
                println!(".L.else.{}:", label);
                self.gen_expr(else_)?;
                println!(".L.end.{}:", label);
            }
            NodeKind::Bin { op, lhs, rhs } => {
                self.gen_expr(lhs)?;
                self.gen_expr(rhs)?;
                println!("  pop rdi"); // rhs
                println!("  pop rax"); // lhs
                let code = match op {
                    BinOp::Add => "  add rax, rdi",
                    BinOp::Sub => "  sub rax, rdi",
                    BinOp::Mul => "  imul rax, rdi",
                    BinOp::Or => "  or rax, rdi",
                    BinOp::And => "  and rax, rdi",
                    BinOp::Xor => "  xor rax, rdi",
                    BinOp::Div => "  cqo\n  idiv rdi", // cqoにより、raxをrdx:raxに符号拡張する(rdx, raxを連結して一つの128ビット用いる), idivでは(rdx:rax)/rdiを実施し、商をrax, あまりをrdxにいれる
                    BinOp::Mod => "  cqo\n  idiv rdi\n  mov rax, rdx",
                    BinOp::_Eq => "  cmp rax, rdi\n  sete al\n  movzb rax, al",
                    BinOp::Neq => "  cmp rax, rdi\n  setne al\n  movzb rax, al",
                    BinOp::Lt => "  cmp rax, rdi\n  setl al\n  movzb rax, al",
                    BinOp::Le => "  cmp rax, rdi\n  setle al\n  movzb rax, al",
                    BinOp::Shl => "  mov rcx, rdi\nshl rax, cl",
                    BinOp::Shr => "  mov rcx, rdi\nsar rax, cl",
                };
                println!("{}", code);
                println!("  push rax")
            }
            _ => {
                return Err(CodeGenError {
                    pos: self.pos,
                    msg: "not an expression!".to_owned(),
                })
            }
        }
        Ok(())
    }
    fn gen_stmt(&mut self, node: &Node) -> Result<(), CodeGenError> {
        self.update_pos(node.tok_no);
        match &node.kind {
            NodeKind::Return(node) => {
                if let Some(node) = node {
                    self.gen_expr(node)?;
                    println!("  pop rax");
                }
                println!("  jmp .L.return.{}", self.func_name);
            }
            NodeKind::Block { stmts } => {
                for stmt in &**stmts {
                    self.gen_stmt(stmt)?;
                }
            }
            NodeKind::ExprStmt(expr) => {
                self.gen_expr(expr)?;
                println!("  pop rax"); // 最後に評価した値を捨てる
            }
            NodeKind::If {
                condi,
                then_,
                else_,
            } => {
                let label = self.new_label();
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
                let label = self.new_label();
                self.break_label.push(label);
                self.continue_label.push(label);
                if let Some(start) = start {
                    self.gen_stmt(start)?;
                }
                println!(".L.begin.{}:", label);
                if let Some(condi) = condi {
                    self.gen_expr(condi)?;
                    println!("  pop rax");
                    println!("  cmp rax, 0");
                    println!("  je .L.break.{}", label);
                }
                self.gen_stmt(loop_)?;
                println!(".L.continue.{}:", label); // インクリメント(end)の手前まですすむ
                if let Some(end) = end {
                    self.gen_stmt(end)?;
                }
                println!("  jmp .L.begin.{}", label);
                println!(".L.break.{}:", label);
                self.break_label.pop();
                self.continue_label.pop();
            }
            NodeKind::Do { stmt, condi } => {
                let label = self.new_label();
                self.break_label.push(label);
                self.continue_label.push(label);
                println!(".L.begin.{}:", label);
                self.gen_stmt(stmt)?;
                println!(".L.continue.{}:", label);
                self.gen_expr(condi)?;
                println!("  pop rax");
                println!("  cmp rax, 0");
                println!("  jne .L.begin.{}", label);
                println!(".L.break.{}:", label);
                self.break_label.pop();
                self.continue_label.pop();
            }
            NodeKind::Break => {
                if let Some(label) = self.break_label.last() {
                    println!("  jmp .L.break.{}", label);
                } else {
                    return Err(CodeGenError {
                        pos: self.pos,
                        msg: "stray break".to_owned(),
                    });
                }
            }
            NodeKind::Continue => {
                if let Some(label) = self.continue_label.last() {
                    println!("  jmp .L.continue.{}", label);
                } else {
                    return Err(CodeGenError {
                        pos: self.pos,
                        msg: "stray continue".to_owned(),
                    });
                }
            }
            NodeKind::Goto(label) => {
                println!("  jmp .L.label.{}.{}", self.func_name, label);
            }
            NodeKind::Label(label, stmt) => {
                println!(".L.label.{}.{}:", self.func_name, label);
                self.gen_stmt(stmt)?;
            }
            NodeKind::Switch {
                condi,
                stmt,
                cases,
                has_default,
            } => {
                let label = self.new_label();
                self.break_label.push(label);
                self.case_label.push(label);
                self.gen_expr(condi)?;
                println!("  pop rax");
                for (i, val) in cases.iter().enumerate() {
                    println!("  cmp rax, {}", val);
                    println!("  je .L.case.{}.{}", label, i);
                }
                if *has_default {
                    println!("  jmp .L.case.{}.default", label);
                } else {
                    println!("  jmp .L.break.{}", label);
                }
                self.gen_stmt(stmt)?;
                println!(".L.break.{}:", label);
                self.break_label.pop();
                self.case_label.pop();
            }
            NodeKind::Case { stmt, id } => {
                println!(
                    ".L.case.{}.{}:",
                    self.case_label[self.case_label.len() - 1],
                    id,
                );
                self.gen_stmt(stmt)?;
            }
            NodeKind::Default_(stmt) => {
                println!(
                    ".L.case.{}.default:",
                    self.case_label[self.case_label.len() - 1]
                );
                self.gen_stmt(stmt)?;
            }
            _ => {
                return Err(CodeGenError {
                    pos: self.pos,
                    msg: "not a statement!".to_owned(),
                })
            }
        }
        Ok(())
    }
}
pub fn code_gen(
    program: Vec<Function>,
    globals: Vec<Rc<Var>>,
    string_literals: Vec<CString>,
    token_list: Vec<Token>,
) -> Result<(), CodeGenError> {
    fn global_var_header(var: &Var) {
        println!(".align {}", var.ty.align());
        if !var.is_static {
            // static local, static globalは.globlをつけない
            println!(".globl {}", var.name);
        }
        println!("{}:", var.global_name());
    }
    let mut cg = CodeGenerator::default();
    cg.tklist = token_list;
    println!(".intel_syntax noprefix");
    println!(".bss");

    for var in &globals {
        if !var.ty.is_func() && var.init_data.is_none() {
            global_var_header(var);
            println!("  .zero {}", var.ty.size());
        }
    }
    println!(".data");
    for (i, s) in string_literals.iter().enumerate() {
        println!(".L.data.{}:", i);
        for c in &s.0 {
            println!("  .byte {}", c);
        }
    }
    for var in &globals {
        if var.ty.is_func() {
            continue; // 関数はここでは宣言しない
        }
        if let Some(data) = &var.init_data {
            global_var_header(var);
            let mut quad_skip = 0; // quad命令が出現すると、+7されて、以下の7個(全てゼロ)はスキップされる。
                                   // これによりquadを8バイトと同様に扱うことができる
            for b in data.iter() {
                if quad_skip > 0 {
                    quad_skip -= 1;
                    continue;
                }
                match b {
                    Data::Byte(b) => println!("  .byte {}", b),
                    Data::Quad(b) => {
                        quad_skip += 7;
                        if b.is_positive {
                            println!("  .quad {}+{}", b.label, b.addend);
                        } else {
                            println!("  .quad {}-{}", b.label, b.addend);
                        }
                    }
                }
            }
        }
    }
    println!(".text");
    for func in program {
        cg.func_name = func.name;
        cg.set_var_offset(&func.locals);
        if !func.is_static {
            println!(".globl {}", cg.func_name);
        }
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
    if let NodeKind::ExprStmt(expr) = &node.kind {
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
        NodeKind::Num(val) => s += &format!("{} [label=\"num {}\"];\n", nodename, val),
        NodeKind::Break => s += &format!("{} [label=\"break\"];\n", nodename),
        NodeKind::Continue => s += &format!("{} [label=\"continue\"];\n", nodename),
        NodeKind::Cast(expr) => {
            s += &format!("{} [label=\"cast {:?}\"];\n", nodename, node.ty);
            s += &graph_gen(expr, &nodename, 0, None);
        }
        NodeKind::BitNot(expr) => {
            s += &format!("{} [label=\"bitnot {:?}\"];\n", nodename, node.ty);
            s += &graph_gen(expr, &nodename, 0, None);
        }
        NodeKind::Var(var) => s += &format!("{} [label=\"var {}\"];\n", nodename, var.name),
        NodeKind::Literal { id, .. } => s += &format!("{} [label=\"literal {}\"];\n", nodename, id),
        NodeKind::Bin { op, lhs, rhs } => {
            s += &format!("{} [label=\"{:?}\"];\n", nodename, op);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::Assign(lhs, rhs) => {
            s += &format!("{} [label=\"assign\"];\n", nodename);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::Comma(lhs, rhs) => {
            s += &format!("{} [label=\"comma\"];\n", nodename);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::LogAnd(lhs, rhs) => {
            s += &format!("{} [label=\"&&\"];\n", nodename);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::LogOr(lhs, rhs) => {
            s += &format!("{} [label=\"||\"];\n", nodename);
            s += &graph_gen(lhs, &nodename, 0, None);
            s += &graph_gen(rhs, &nodename, 1, None);
        }
        NodeKind::Return(node) => {
            s += &format!("{} [label=\"return\"];\n", nodename);
            if let Some(node) = node {
                s += &graph_gen(node, &nodename, number, None);
            }
        }
        NodeKind::ExprStmt { .. } => panic!(),
        NodeKind::Addr(node) => {
            s += &format!("{} [label=\"addr\"];\n", nodename);
            s += &graph_gen(node, &nodename, 0, None);
        }
        NodeKind::Deref(node) => {
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
        NodeKind::Do { stmt, condi } => {
            s += &format!("{} [label=\"do\"];\n", nodename);
            s += &graph_gen(stmt, &nodename, 0, Some("stmt"));
            s += &graph_gen(condi, &nodename, 1, Some("condi"));
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
        _ => (),
    }
    s
}
