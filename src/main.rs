use std::io::{self, Write};

// 假设这些是您已有的模块和结构体
use litec_ast::token::TokenKind;
use litec_parse::lexer::Lexer;

fn main() {
    println!("Rust 词法分析器演示");
    println!("输入源代码 (输入 ':quit' 退出):");
    
    loop {
        // 打印提示符
        print!("> ");
        io::stdout().flush().unwrap();
        
        // 读取用户输入
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                
                // 检查退出命令
                if input == ":quit" {
                    println!("再见!");
                    break;
                }
                
                // 跳过空输入
                if input.is_empty() {
                    continue;
                }
                
                // 创建词法分析器并处理输入
                let mut lexer = Lexer::new(input);
                
                println!("Token 流:");
                println!("{:<15} {:<20} {:<10} {}", "Kind", "Text", "Span", "Value");
                println!("{}", "-".repeat(60));
                
                // 循环获取并打印所有 token
                loop {
                    let token = lexer.next_token();
                    
                    // 格式化输出
                    let kind_str = format!("{:?}", token.kind);
                    let text_str = format!("'{}'", token.text);
                    let span_str = format!("{:?}", token.span);
                    
                    // 对于字面量 token，尝试提取值
                    let value_str = match token.kind {
                        TokenKind::Literal { kind, suffix_start: _ } => {
                            format!("{:?}", kind)
                        }
                        _ => "".to_string(),
                    };
                    
                    println!("{:<15} {:<20} {:<10} {}", kind_str, text_str, span_str, value_str);
                    
                    // 检查是否到达文件末尾
                    if matches!(token.kind, TokenKind::Eof) {
                        break;
                    }
                }
                
                println!(); // 空行分隔不同输入
            }
            Err(error) => {
                eprintln!("读取输入时出错: {}", error);
                break;
            }
        }
    }
}