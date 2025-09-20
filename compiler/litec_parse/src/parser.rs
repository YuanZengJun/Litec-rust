use litec_errors::ParseError;
use litec_ast::{ast::{Expr, Function, Item, Param, Stmt, TypeAnnotation}, token::{LiteralKind, Token, TokenKind}};
use litec_span::intern_global;
use crate::lexer::Lexer;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Assignment = 1,   // =
    LogicalOr = 2,    // ||
    LogicalAnd = 3,   // &&
    Equality = 4,     // ==, !=
    Comparison = 5,   // <, >, <=, >=
    Term = 6,         // +, -
    Factor = 7,       // *, /, %
    Unary = 8,        // !
    Call = 9,         // 函数调用
    Member = 10,      // ., []
}

impl Precedence {
    fn from_token_kind(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Eq => Precedence::Assignment,
            TokenKind::Or => Precedence::LogicalOr,
            TokenKind::And => Precedence::LogicalAnd,
            TokenKind::EqEq | TokenKind::NotEq => Precedence::Equality,
            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => Precedence::Comparison,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            TokenKind::Bang => Precedence::Unary, // 前缀运算符
            TokenKind::OpenParen => Precedence::Call,
            TokenKind::Dot | TokenKind::OpenBracket => Precedence::Member,
            _ => Precedence::Lowest,
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Token<'src>,
    errors: Vec<ParseError>
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        let mut lexer = Lexer::new(source);
        match lexer.advance_token() {
            Ok(current_token) => {
                Parser {
                    lexer: lexer,
                    current_token: current_token,
                    errors: Vec::new()
                }
            }
            Err(err) => {
                Parser {
                    lexer: lexer,
                    current_token: Token { span: err.span(), kind: TokenKind::Error, text: "" },
                    errors: vec![err]
                }
            }
        }
    }

    fn advance(&mut self) {
        loop {
            match self.lexer.advance_token() {
                Ok(token) => {
                    self.current_token = token;
                    return;
                }
                Err(err) => {
                    self.errors.push(err);
                    self.recover();
                }
            }
        }
    }

    #[inline]
    fn expect(&mut self, kind: TokenKind, err: ParseError) -> Result<Token<'src>, ParseError> {
        if self.current_token.kind == kind {
            let token = self.current_token.clone();
            self.advance();
            Ok(token)
        } else {
            Err(err)
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Item>, Vec<ParseError>> {
        let mut statements = Vec::new();
        
        while self.current_token.kind != TokenKind::Eof {
            match self.parse_item() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    // 尝试恢复，寻找下一个语句的开始
                    self.recover();
                }
            }
        }
        
        if self.errors.is_empty() {
            Ok(statements)
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        match self.current_token.kind {
            TokenKind::Fn => self.parse_fn_item(),
            _ => Err(ParseError::ExpectedItem { found: self.current_token.kind.clone(), span: self.current_token.span })
        }
    }

    fn parse_fn_item(&mut self) -> ParseResult<Item> {
        let function = self.parse_fn()?;

        Ok(Item::Function(function))
    }

    fn parse_fn(&mut self) -> ParseResult<Function> {
        let span = self.current_token.span;
        self.advance();

        let name = self.expect(TokenKind::Ident, ParseError::ExpectedIdentifier { found: self.current_token.kind.clone() , span: self.current_token.span })?;

        self.expect(TokenKind::OpenParen, ParseError::ExpectedOpenBrace { found: self.current_token.kind.clone() , span: self.current_token.span })?;

        let mut params: Vec<Param> = Vec::new();
        if self.current_token.kind != TokenKind::CloseParen {
            loop {
                params.push(self.parse_param()?);

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::CloseParen, ParseError::ExpectedOpenBrace { found: self.current_token.kind.clone() , span: self.current_token.span })?;

        let mut return_type: Option<TypeAnnotation> = None;
        if self.eat(TokenKind::Arrow) {
            return_type = Some(self.parse_type()?);
        }

        let block = self.parse_block()?;
        let block_span = block.span();

        Ok(Function {
            name: intern_global(name.text),
            return_type: return_type,
            params: params,
            body: block,
            span: span.extend_to(block_span)
        })
    }

    fn parse_param(&mut self) -> ParseResult<Param> {
        let name = self.expect(TokenKind::Ident, ParseError::ExpectedIdentifier { found: self.current_token.kind.clone(), span: self.current_token.span })?;
        self.expect(TokenKind::Colon, ParseError::ExpectedColon { found: self.current_token.kind.clone(), span: self.current_token.span })?;
        let r#type = self.parse_type()?;
        let name_span = name.span;
        let r#type_span = r#type.span();

        Ok(Param {
            name: intern_global(name.text),
            r#type: r#type,
            span: name_span.extend_to(r#type_span)
        })
    }

    fn parse_type(&mut self) -> ParseResult<TypeAnnotation> {
        let start_span = self.current_token.span;
        
        match &self.current_token.kind {
            TokenKind::Ident => {
                // 解析标识符类型
                let token = self.current_token.clone();
                self.advance();
                Ok(TypeAnnotation::Ident {
                    name: intern_global(token.text),
                    span: start_span.extend_to(token.span),
                })
            }
            // 可以添加更多类型解析逻辑
            _ => Err(ParseError::ExpectedType {
                found: self.current_token.kind.clone(),
                span: self.current_token.span,
            }),
        }
    }

    fn recover(&mut self) {
        // 记录开始恢复的位置
        let recovery_start = self.current_token.span.start();
        
        // 尝试找到下一个语句边界
        let mut nesting_level = 0; // 用于跟踪括号/大括号的嵌套层级
        
        while self.current_token.kind != TokenKind::Eof {
            match self.current_token.kind {
                // 增加嵌套层级的 token
                TokenKind::OpenParen | TokenKind::OpenBrace | TokenKind::OpenBracket => {
                    nesting_level += 1;
                }
                // 减少嵌套层级的 token
                TokenKind::CloseParen | TokenKind::CloseBrace | TokenKind::CloseBracket => {
                    if nesting_level > 0 {
                        nesting_level -= 1;
                    } else {
                        // 额外的关闭符号，可能是恢复点
                        self.advance();
                        break;
                    }
                }
                // 语句结束符号
                TokenKind::Semi if nesting_level == 0 => {
                    // 在顶层分号处恢复
                    self.advance();
                    break;
                }
                // 语句开始的关键字
                TokenKind::Fn | TokenKind::Let | TokenKind::If | TokenKind::While | 
                TokenKind::For | TokenKind::Return if nesting_level == 0 => {
                    // 在语句开始处恢复
                    break;
                }
                _ => {}
            }

            // 前进到下一个 token
            self.advance();

            // 安全机制：如果移动了太远而没有找到恢复点，强制停止
            if self.current_token.span.start() - recovery_start > 1000 {
                // 移动了超过1000个字符，强制停止恢复
                break;
            }
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_expression_with_precedence(Precedence::Lowest)
    }

    fn parse_expression_with_precedence(&mut self, precedence: Precedence) -> ParseResult<Expr> {
        let mut left = self.parse_prefix()?;
        
        while self.current_token.kind != TokenKind::Eof 
            && self.current_token.kind != TokenKind::Semi
            && precedence < Precedence::from_token_kind(&self.current_token.kind)
        {
            left = self.parse_infix(left)?;
        }
        
        Ok(left)
    }

    fn parse_prefix(&mut self) -> ParseResult<Expr> {
        match &self.current_token.kind {
            TokenKind::Literal { kind, suffix } => {
                if let LiteralKind::Char { terminated } = kind {
                    if !terminated {
                        return Err(ParseError::UnterminatedChar {
                            span: self.current_token.span,
                        });
                    }
                }
                
                if let LiteralKind::Str { terminated } = kind {
                    if !terminated {
                        return Err(ParseError::UnterminatedString {
                            span: self.current_token.span,
                        });
                    }
                }
                
                let value = intern_global(self.current_token.text);
                let suffix_id = suffix.as_ref().map(|s| intern_global(s));
                
                let expr = Expr::Literal {
                    kind: kind.clone(),
                    value,
                    suffix: suffix_id,
                    span: self.current_token.span,
                };
                
                self.advance();
                Ok(expr)
            }
            TokenKind::Ident => {
                let name = intern_global(self.current_token.text);
                let span = self.current_token.span;
                
                self.advance();
                Ok(Expr::Ident { name, span })
            }
            TokenKind::OpenParen => {
                let start_span = self.current_token.span;
                self.advance();
                
                let expr = self.parse_expression()?;
                
                self.expect(TokenKind::CloseParen, ParseError::UnclosedParenthesis { span: self.current_token.span })?;
                
                let span = start_span.extend_to(self.current_token.span);
                Ok(Expr::Grouped {
                    expr: Box::new(expr),
                    span,
                })
            }
            TokenKind::Bang | TokenKind::Minus => {
                let op = self.current_token.kind.clone();
                let start_span = self.current_token.span;
                
                self.advance();
                
                let expr = self.parse_expression_with_precedence(Precedence::Unary)?;
                
                let span = start_span.extend_to(expr.span());
                Ok(Expr::Unary {
                    op,
                    expr: Box::new(expr),
                    span,
                })
            }
            TokenKind::If => self.parse_if_expression(),
            TokenKind::While => self.parse_while_expression(),
            TokenKind::For => self.parse_for_expression(),
            TokenKind::OpenBrace => self.parse_block(),

            _ => Err(ParseError::ExpectedExpression {
                found: self.current_token.kind.clone(),
                span: self.current_token.span,
            }),
        }
    }

    fn parse_infix(&mut self, left: Expr) -> ParseResult<Expr> {
        match self.current_token.kind {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::EqEq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::And
            | TokenKind::Or => self.parse_binary_expression(left),
            
            TokenKind::Eq => self.parse_assignment_expression(left),

            TokenKind::OpenParen => self.parse_call_exprssion(left),
            
            _ => Ok(left), // 不是中缀运算符，直接返回左表达式
        }
    }

    fn parse_call_exprssion(&mut self, callee: Expr) -> ParseResult<Expr> {
        let span = self.current_token.span;
        self.advance();

        let mut arguments: Vec<Expr> = Vec::new();
        while self.current_token.kind != TokenKind::CloseParen && self.current_token.kind != TokenKind::Eof {
            arguments.push(self.parse_expression()?);

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        let close = self.expect(TokenKind::CloseParen, ParseError::UnclosedParenthesis { span: self.current_token.span })?.span;

        Ok(Expr::Call { callee: Box::new(callee), args: arguments, span: span.extend_to(close) })
    }

    fn parse_binary_expression(&mut self, left: Expr) -> ParseResult<Expr> {
        let op = self.current_token.kind.clone();
        let precedence = Precedence::from_token_kind(&op);
        
        self.advance();
        
        let right = self.parse_expression_with_precedence(precedence)?;
        
        let span = left.span().extend_to(right.span());
        Ok(Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span,
        })
    }

    fn parse_assignment_expression(&mut self, left: Expr) -> ParseResult<Expr> {
        // 检查赋值目标是否有效
        if !matches!(left, Expr::Ident { .. } | Expr::Call { .. }) {
            return Err(ParseError::InvalidAssignmentTarget {
                span: left.span(),
            });
        }
        
        let op = self.current_token.kind.clone();
        let start_span = left.span();
        
        self.advance();
        
        let value = self.parse_expression_with_precedence(Precedence::Assignment)?;
        
        let span = start_span.extend_to(value.span());
        Ok(Expr::Assignment {
            target: Box::new(left),
            op,
            value: Box::new(value),
            span,
        })
    }

    fn parse_if_expression(&mut self) -> ParseResult<Expr> {
        let start_span = self.current_token.span;
        self.advance(); // 消耗 'if'
        
        let condition = self.parse_expression()?;
        
        let then_branch = self.parse_block()?;
        
        let else_branch = if self.current_token.kind == TokenKind::Else {
            self.advance(); // 消耗 'else'
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        let mut span = start_span.extend_to(then_branch.span());
        if let Some(else_branch) = &else_branch {
            span = span.extend_to(else_branch.span());
        }
        
        Ok(Expr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
            span,
        })
    }

    fn parse_while_expression(&mut self) -> ParseResult<Expr> {
        let start_span = self.current_token.span;
        self.advance(); // 消耗 'while'
        
        let condition = self.parse_expression()?;
        
        let body = self.parse_block()?;
        
        let span = start_span.extend_to(body.span());
        Ok(Expr::While {
            condition: Box::new(condition),
            body: Box::new(body),
            span,
        })
    }

    fn parse_for_expression(&mut self) -> ParseResult<Expr> {
        let start_span = self.current_token.span;
        self.advance(); // 消耗 'for'
        
        // 解析迭代变量
        let variable = match self.parse_expression()? {
            Expr::Ident { name, span } => (name, span),
            _ => return Err(ParseError::ExpectedIdentifier {
                found: self.current_token.kind.clone(),
                span: self.current_token.span,
            }),
        };
        
        // 检查 'in' 关键字
        self.expect(TokenKind::In, ParseError::ExpectedIn {
            found: self.current_token.kind.clone(),
            span: self.current_token.span,
        })?;
        
        // 解析生成器表达式
        let generator = self.parse_expression()?;
        
        // 解析循环体
        let body = self.parse_block()?;
        
        let span = start_span.extend_to(body.span());
        Ok(Expr::For {
            variable: Box::new(Expr::Ident {
                name: variable.0,
                span: variable.1,
            }),
            generator: Box::new(generator),
            body: Box::new(body),
            span,
        })
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.expect(TokenKind::OpenBrace, ParseError::ExpectedOpenBrace {
            found: self.current_token.kind.clone(),
            span: self.current_token.span,
        })?;
    
        let mut statements = Vec::new();
        let mut tail: Option<Box<Expr>> = None;
    
        while self.current_token.kind != TokenKind::CloseBrace && self.current_token.kind != TokenKind::Eof {
            match self.current_token.kind {
                TokenKind::LineComment | TokenKind::BlockComment => {
                    self.advance();
                    continue;
                }
                TokenKind::Let => {
                    statements.push(self.parse_let_statement()?);
                    self.expect(TokenKind::Semi, ParseError::ExpectedSemi { found: self.current_token.kind.clone(), span: self.current_token.span })?;
                }
                TokenKind::Return => {
                    statements.push(self.parse_return_statement()?);
                    self.expect(TokenKind::Semi, ParseError::ExpectedSemi { found: self.current_token.kind.clone(), span: self.current_token.span })?;
                }
                _ => {
                    let expr = Box::new(self.parse_expression()?);
                    if self.current_token.kind == TokenKind::CloseBrace {
                        tail = Some(expr);
                    } else {
                        self.expect(TokenKind::Semi, ParseError::ExpectedSemi { found: self.current_token.kind.clone(), span: self.current_token.span })?;
                        let expr_span = expr.span();
                        statements.push(Stmt::Expr { expr: expr, span: expr_span });
                    }
                }
            }
        }
    
        let close_brace = self.expect(TokenKind::CloseBrace, ParseError::UnclosedBrace { span: self.current_token.span })?;
    
        Ok(Expr::Block { 
            stmts: statements,
            tail: tail,
            span: start.extend_to(close_brace.span)
        })
    }

    fn parse_let_statement(&mut self) -> ParseResult<Stmt> {
        let span = self.current_token.span;
        self.advance();
        let mut has_type = false;
        let mut has_value = false;

        let name = self.expect(TokenKind::Ident, ParseError::ExpectedIdentifier { found: self.current_token.kind.clone(), span: self.current_token.span })?;

        let mut r#type: Option<TypeAnnotation> = None;
        if self.eat(TokenKind::Colon) {
            r#type = Some(self.parse_type()?);
            has_type = true;
        }

        let mut value: Option<Expr> = None;
        if self.eat(TokenKind::Eq) {
            value = Some(self.parse_expression()?);
            has_value = true;
        }

        if has_value {
            let value_span = value.clone().unwrap().span();
            Ok(Stmt::Let {
                name: intern_global(name.text), 
                r#type: r#type, 
                value: value, 
                span: span.extend_to(value_span) 
            })
        } else if has_type {
            let type_span = r#type.clone().unwrap().span();
            Ok(Stmt::Let { name: intern_global(name.text), r#type: r#type, value: value, span: span.extend_to(type_span) })
        } else {
            Ok(Stmt::Let { name: intern_global(name.text), r#type: r#type, value: value, span: span.extend_to(name.span) })
        }
    }

    fn parse_return_statement(&mut self) -> ParseResult<Stmt> {
        let span = self.current_token.span;
        self.advance();
        let mut has_value= false;

        let mut value: Option<Expr> = None;
        if self.current_token.kind != TokenKind::Semi {
            value = Some(self.parse_expression()?);
            has_value = true;
        }
        
        let span = if has_value {
            span.extend_to(value.clone().unwrap().span())
        } else {
            span
        };

        Ok(Stmt::Return { value: value, span: span })
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.current_token.kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }
}