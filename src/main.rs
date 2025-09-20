use std::io::{self, Write};
use litec_ast::token::TokenKind;
use litec_parse::lexer::Lexer;
use litec_parse::parser::Parser;

fn main() {
    println!("Rust Parser REPL Demo");
    println!("Enter source code (type ':quit' to exit, ':tokens' to show lexer results):");
    
    loop {
        // Print prompt
        print!("> ");
        io::stdout().flush().unwrap();
        
        // Read user input
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                
                // Check for quit command
                if input == ":quit" {
                    println!("Goodbye!");
                    break;
                }
                
                // Check for token display command
                if input == ":tokens" {
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
                    continue;
                }
                
                // Skip empty input
                if input.is_empty() {
                    continue;
                }
                
                // Parse input
                parse_and_display(input);
            }
            Err(error) => {
                eprintln!("Error reading input: {}", error);
                break;
            }
        }
    }
}

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
    println!("\nParsing Results:");
    
    // Create parser
    let mut parser = Parser::new(input);
    
    let result = parser.parse();

    // Parse input
    match result {
        Ok(_) => {            
            println!("{:#?}", result);
        }
        Err(errors) => {
            println!("Parse failed");
            
            for error in errors {
                println!("    {} SourceRange:'{}'", error, error.span().extract(input).unwrap());
            }
        }
    }
    
    println!(); // Empty line separator
}