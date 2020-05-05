use crate::parse::Node;

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
