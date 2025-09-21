use std::io::{self, Write};
use colored::Colorize;
use litec_ast::token::TokenKind;
use litec_parse::lexer::Lexer;
use litec_parse::parser::Parser;
use litec_span::GLOBAL_STRING_POOL;
use litec_lower::Lower;

fn main() {
    println!("Rust Parser REPL Demo");
    println!("Enter source code (type ':quit' to exit, ':tokens' to show lexer results, ':hir' to show HIR, ':clear' to clear input):");
    println!("You can paste multi-line code directly.");
    
    let mut multi_line_input = String::new();
    let mut waiting_for_hir = false; // 新增：标记是否等待输入:hir

    loop {
        // Print prompt
        if multi_line_input.is_empty() {
            print!("> ");
        } else {
            print!(". ");
        }
        io::stdout().flush().unwrap();
        
        // Read user input
        let mut input_line = String::new();
        match io::stdin().read_line(&mut input_line) {
            Ok(0) => {
                // EOF (Ctrl+D) encountered
                if !multi_line_input.is_empty() {
                    // Process any pending input
                    parse_and_display(&multi_line_input);
                    multi_line_input.clear();
                }
                println!("\nGoodbye!");
                break;
            }
            Ok(_) => {
                let input_line = input_line.trim_end();
                
                // Check for quit command
                if input_line == ":quit" {
                    println!("Goodbye!");
                    break;
                }
                
                // Check for clear command
                if input_line == ":clear" {
                    multi_line_input.clear();
                    println!("Input cleared.");
                    waiting_for_hir = false;
                    continue;
                }
                
                // Check for token display command
                if input_line == ":tokens" {
                    if !multi_line_input.is_empty() {
                        println!("Current multi-line input will be used for lexical analysis.");
                        show_tokens(&multi_line_input);
                        waiting_for_hir = false;
                    } else {
                        println!("Please enter code for lexical analysis:");
                        print!("> ");
                        io::stdout().flush().unwrap();
                        
                        let mut code_input = String::new();
                        if io::stdin().read_line(&mut code_input).is_ok() {
                            let code_input = code_input.trim();
                            if !code_input.is_empty() {
                                show_tokens(code_input);
                            }
                        }
                    }
                    continue;
                }
                
                // Check for HIR display command
                if input_line == ":hir" {
                    if !multi_line_input.is_empty() {
                        println!("\nHIR Representation:");
                        print_hir(&multi_line_input);
                        multi_line_input.clear();
                        waiting_for_hir = false;
                    } else {
                        println!("Please enter code first (or paste multi-line code).");
                        println!("Type ':hir' after entering the code.");
                        waiting_for_hir = true;
                    }
                    continue;
                }
                
                // 如果正在等待:hir命令，直接添加到输入
                if waiting_for_hir {
                    // 添加输入到multi_line_input
                    multi_line_input.push_str(input_line);
                    multi_line_input.push('\n');
                    continue;
                }
                
                // Skip empty input unless we're in multi-line mode
                if input_line.is_empty() {
                    if !multi_line_input.is_empty() {
                        // 不自动解析，等待用户输入命令
                        println!("Please type ':hir' to show HIR or ':tokens' to show tokens.");
                    }
                    continue;
                }
                
                // Add the current line to multi-line input
                multi_line_input.push_str(input_line);
                multi_line_input.push('\n');
                
                // 不再自动解析，等待用户输入命令
                // if is_input_complete(&multi_line_input) { ... }
            }
            Err(error) => {
                eprintln!("Error reading input: {}", error);
                break;
            }
        }
    }
}

// 以下函数保持不变
fn show_tokens(input: &str) {
    println!("\nLexical Analysis Results:");
    println!("{:<20} {:<15} {:<10} {}", "Kind", "Text", "Span", "Details");
    println!("{}", "-".repeat(70));
    
    let mut lexer = Lexer::new(input);
    let mut errors = Vec::new();
    
    loop {
        match lexer.advance_token() {
            Ok(token) => {
                let kind_str = format!("{:?}", token.kind);
                let text_str = if token.text.len() > 12 {
                    format!("'{}...'", &token.text[..10])
                } else {
                    format!("'{}'", token.text)
                };
                let span_str = format!("{}..{}", token.span.start(), token.span.end());
                
                // Show details for literal tokens
                let details = match &token.kind {
                    TokenKind::Literal { kind, suffix } => {
                        format!("Kind: {:?}, Suffix: {:?}", kind, suffix)
                    }
                    _ => String::new(),
                };
                
                println!("{:<20} {:<15} {:<10} {}", kind_str, text_str, span_str, details);
                
                // Check if we've reached end of file
                if matches!(token.kind, TokenKind::Eof) {
                    break;
                }
            }
            Err(error) => {
                errors.push(error);
                // Try to recover by skipping current character
                lexer.advance(1);
            }
        }
    }
    
    // Show errors
    if !errors.is_empty() {
        println!("\nLexical Errors:");
        for error in &errors {
            println!("  {}", error);
        }
    }
    
    println!(); // Empty line separator
}

fn parse_and_display(input: &str) {
    // Create parser
    let mut parser = Parser::new(input);
    
    let result = parser.parse();

    // Parse input
    match result {
        Ok(ast) => {            
            println!("{}", format!("{:#?}", ast).cyan());
            println!("{}", GLOBAL_STRING_POOL.to_string());
        }
        Err(errors) => {
            println!("Parse failed");
            
            for error in errors {
                // Extract the relevant part of the source for error reporting
                let span = error.span();
                let source_snippet = if span.start() < input.len() && span.end() <= input.len() {
                    let start = span.start().saturating_sub(10);
                    let end = std::cmp::min(span.end() + 10, input.len());
                    format!("{}", &input[start..end])
                } else {
                    "unknown location".to_string()
                };
                
                println!("{}", format!("    {} Near: '{}'", error, source_snippet).red());
            }
        }
    }
    
    println!(); // Empty line separator
}

// 新增：打印HIR的函数
fn print_hir(input: &str) {
    // 解析为AST
    let mut parser = Parser::new(input);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            eprintln!("Parsing failed for HIR:");
            for error in errors {
                eprintln!("  {}", error);
            }
            return;
        }
    };

    let mut lower = Lower::new();
    // 将AST转换为HIR
    let hir = match lower.lower_crate(ast) {
        Ok(hir) => hir,
        Err(errors) => {
            eprintln!("Lowering failed:");
            for error in errors {
                eprintln!("  {}", error);
            }
            return;
        }
    };

    // 打印HIR
    println!("{}", format!("{:#?}", hir).cyan());
    
    // 打印字符串池（可选）
    println!("\nString Pool:");
    println!("{}", GLOBAL_STRING_POOL.to_string());
}