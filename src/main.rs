use std::env;

fn main() {
    match env::args().nth(1) {
        Some(x) => println!(
            ".intel_syntax noprefix
.global main
main:
  mov rax, {}
  ret      
",
            x
        ),
        None => eprintln!("引数の数が正しくありません。"),
    }
}
