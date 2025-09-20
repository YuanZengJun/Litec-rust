use litec_ast::token::TokenKind;
use litec_span::Span;
use thiserror::Error;

/// 统一的解析错误类型（包含词法和语法错误）
#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseError {
    // 词法错误
    #[error("Unterminated character literal at {span:?}")]
    UnterminatedChar { span: Span },
    
    #[error("Unterminated string literal at {span:?}")]
    UnterminatedString { span: Span },
    
    #[error("Empty integer literal at {span:?}")]
    EmptyInteger { span: Span },
    
    #[error("Empty exponent in float literal at {span:?}")]
    EmptyExponent { span: Span },
    
    #[error("Invalid character '{char}' at {span:?}")]
    InvalidCharacter { char: char, span: Span },
    
    #[error("Invalid identifier at {span:?}")]
    InvalidIdentifier { span: Span },
    
    #[error("Unexpected end of file at {span:?}")]
    UnexpectedEof { span: Span },
    
    // 语法错误
    #[error("Unexpected token: expected {expected:?}, found {found:?} at {span:?}")]
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected identifier, found {found:?} at {span:?}")]
    ExpectedIdentifier {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected assignment operator, found {found:?} at {span:?}")]
    ExpectedAssign {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected semicolon, found {found:?} at {span:?}")]
    ExpectedSemi {
        found: TokenKind,
        span: Span,
    },

    #[error("Excected open_brace, found {found:?} at {span:?}")]
    ExpectedOpenBrace {
        found: TokenKind,
        span: Span
    },

    #[error("Excected open_paren, found {found:?} at {span:?}")]
    ExpectedOpenParen {
        found: TokenKind,
        span: Span
    },

    #[error("Excected in, found {found:?} at {span:?}")]
    ExpectedIn {
        found: TokenKind,
        span: Span
    },

    #[error("Excected item, found {found:?} at {span:?}")]
    ExpectedItem {
        found: TokenKind,
        span: Span
    },

    #[error("Excected type, found {found:?} at {span:?}")]
    ExpectedType {
        found: TokenKind,
        span: Span
    },
    
    #[error("Unclosed parenthesis at {span:?}")]
    UnclosedParenthesis { span: Span },
    
    #[error("Unclosed brace at {span:?}")]
    UnclosedBrace { span: Span },
    
    #[error("Unclosed bracket at {span:?}")]
    UnclosedBracket { span: Span },
    
    #[error("Invalid integer: {text} at {span:?}")]
    InvalidInteger {
        text: String,
        span: Span,
    },
    
    #[error("Invalid float: {text} at {span:?}")]
    InvalidFloat {
        text: String,
        span: Span,
    },
    
    #[error("Invalid token at {span:?}")]
    InvalidToken { span: Span },
    
    #[error("Expected expression, found {found:?} at {span:?}")]
    ExpectedExpression {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected statement, found {found:?} at {span:?}")]
    ExpectedStatement {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected type annotation, found {found:?} at {span:?}")]
    ExpectedTypeAnnotation {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected block, found {found:?} at {span:?}")]
    ExpectedBlock {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected block or if after else, found {found:?} at {span:?}")]
    ExpectedBlockOrIf {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Duplicate parameter name '{name}' at {span:?}")]
    DuplicateParameter {
        name: String,
        span: Span,
    },
    
    #[error("Too many function parameters at {span:?}")]
    TooManyParameters { span: Span },
    
    #[error("Invalid assignment target at {span:?}")]
    InvalidAssignmentTarget { span: Span },
    
    #[error("Break statement outside loop at {span:?}")]
    BreakOutsideLoop { span: Span },
    
    #[error("Continue statement outside loop at {span:?}")]
    ContinueOutsideLoop { span: Span },
    
    #[error("Return statement outside function at {span:?}")]
    ReturnOutsideFunction { span: Span },
    
    #[error("If expression without else branch at {span:?}")]
    IfWithoutElse { span: Span },
    
    #[error("Expected condition after if/while, found {found:?} at {span:?}")]
    ExpectedCondition {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected loop body at {span:?}")]
    ExpectedLoopBody { span: Span },
    
    #[error("Expected function body at {span:?}")]
    ExpectedFunctionBody { span: Span },
    
    #[error("Expected variable name, found {found:?} at {span:?}")]
    ExpectedVariableName {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected colon after variable name, found {found:?} at {span:?}")]
    ExpectedColon {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Expected comma between parameters, found {found:?} at {span:?}")]
    ExpectedComma {
        found: TokenKind,
        span: Span,
    },
    
    #[error("Unexpected comma at {span:?}")]
    UnexpectedComma { span: Span },
    
    #[error("Invalid binary operation at {span:?}")]
    InvalidBinaryOperation { span: Span },
    
    #[error("Invalid unary operation at {span:?}")]
    InvalidUnaryOperation { span: Span },
    
    #[error("Invalid literal suffix '{suffix}' at {span:?}")]
    InvalidLiteralSuffix {
        suffix: String,
        span: Span,
    },
    
    #[error("Type mismatch: expected {expected}, found {found} at {span:?}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    
    #[error("Cannot use '{operator}' with types {left} and {right} at {span:?}")]
    InvalidOperatorTypes {
        operator: String,
        left: String,
        right: String,
        span: Span,
    },
    
    #[error("Variable '{name}' not found in this scope at {span:?}")]
    VariableNotFound {
        name: String,
        span: Span,
    },
    
    #[error("Function '{name}' not found at {span:?}")]
    FunctionNotFound {
        name: String,
        span: Span,
    },
    
    #[error("Cannot assign to {target} at {span:?}")]
    CannotAssignTo {
        target: String,
        span: Span,
    },
    
    #[error("Invalid number of arguments: expected {expected}, found {found} at {span:?}")]
    InvalidArgumentCount {
        expected: usize,
        found: usize,
        span: Span,
    },
    
    #[error("Missing return statement in function at {span:?}")]
    MissingReturn { span: Span },
    
    #[error("Invalid return type: expected {expected}, found {found} at {span:?}")]
    InvalidReturnType {
        expected: String,
        found: String,
        span: Span,
    },
    
    #[error("Division by zero at {span:?}")]
    DivisionByZero { span: Span },
    
    #[error("Constant expression required at {span:?}")]
    ConstantExpressionRequired { span: Span },
    
    #[error("Recursive type definition at {span:?}")]
    RecursiveType { span: Span },
    
    #[error("Circular dependency detected at {span:?}")]
    CircularDependency { span: Span },
    
    #[error("Import not found: '{name}' at {span:?}")]
    ImportNotFound {
        name: String,
        span: Span,
    },
    
    #[error("Module '{name}' not found at {span:?}")]
    ModuleNotFound {
        name: String,
        span: Span,
    },
    
    #[error("Invalid module path '{path}' at {span:?}")]
    InvalidModulePath {
        path: String,
        span: Span,
    },
    
    #[error("Access control violation at {span:?}")]
    AccessControlViolation { span: Span },
    
    #[error("Feature '{feature}' not supported at {span:?}")]
    FeatureNotSupported {
        feature: String,
        span: Span,
    },
    
    #[error("Internal compiler error: {message} at {span:?}")]
    InternalError {
        message: String,
        span: Span,
    },
}

impl ParseError {
    /// 获取错误的 span
    pub fn span(&self) -> Span {
        match self {
            // 词法错误
            ParseError::UnterminatedChar { span } => *span,
            ParseError::UnterminatedString { span } => *span,
            ParseError::EmptyInteger { span } => *span,
            ParseError::EmptyExponent { span } => *span,
            ParseError::InvalidCharacter { span, .. } => *span,
            ParseError::InvalidIdentifier { span } => *span,
            ParseError::UnexpectedEof { span } => *span,
            
            // 语法错误
            ParseError::UnexpectedToken { span, .. } => *span,
            ParseError::ExpectedIdentifier { span, .. } => *span,
            ParseError::ExpectedAssign { span, .. } => *span,
            ParseError::ExpectedSemi { span, .. } => *span,
            ParseError::UnclosedParenthesis { span } => *span,
            ParseError::UnclosedBrace { span } => *span,
            ParseError::UnclosedBracket { span } => *span,
            ParseError::InvalidInteger { span, .. } => *span,
            ParseError::InvalidFloat { span, .. } => *span,
            ParseError::InvalidToken { span } => *span,
            ParseError::ExpectedExpression { span, .. } => *span,
            ParseError::ExpectedStatement { span, .. } => *span,
            ParseError::ExpectedTypeAnnotation { span, .. } => *span,
            ParseError::ExpectedBlock { span, .. } => *span,
            ParseError::ExpectedBlockOrIf { span, .. } => *span,
            ParseError::DuplicateParameter { span, .. } => *span,
            ParseError::TooManyParameters { span } => *span,
            ParseError::InvalidAssignmentTarget { span } => *span,
            ParseError::BreakOutsideLoop { span } => *span,
            ParseError::ContinueOutsideLoop { span } => *span,
            ParseError::ReturnOutsideFunction { span } => *span,
            ParseError::IfWithoutElse { span } => *span,
            ParseError::ExpectedCondition { span, .. } => *span,
            ParseError::ExpectedLoopBody { span } => *span,
            ParseError::ExpectedFunctionBody { span } => *span,
            ParseError::ExpectedVariableName { span, .. } => *span,
            ParseError::ExpectedColon { span, .. } => *span,
            ParseError::ExpectedComma { span, .. } => *span,
            ParseError::UnexpectedComma { span } => *span,
            ParseError::InvalidBinaryOperation { span } => *span,
            ParseError::InvalidUnaryOperation { span } => *span,
            ParseError::InvalidLiteralSuffix { span, .. } => *span,
            ParseError::TypeMismatch { span, .. } => *span,
            ParseError::InvalidOperatorTypes { span, .. } => *span,
            ParseError::VariableNotFound { span, .. } => *span,
            ParseError::FunctionNotFound { span, .. } => *span,
            ParseError::CannotAssignTo { span, .. } => *span,
            ParseError::InvalidArgumentCount { span, .. } => *span,
            ParseError::MissingReturn { span } => *span,
            ParseError::InvalidReturnType { span, .. } => *span,
            ParseError::DivisionByZero { span } => *span,
            ParseError::ConstantExpressionRequired { span } => *span,
            ParseError::RecursiveType { span } => *span,
            ParseError::CircularDependency { span } => *span,
            ParseError::ImportNotFound { span, .. } => *span,
            ParseError::ModuleNotFound { span, .. } => *span,
            ParseError::InvalidModulePath { span, .. } => *span,
            ParseError::AccessControlViolation { span } => *span,
            ParseError::FeatureNotSupported { span, .. } => *span,
            ParseError::InternalError { span, .. } => *span,
            ParseError::ExpectedIn { span, .. } => *span,
            ParseError::ExpectedOpenBrace { span, .. } => *span,
            ParseError::ExpectedItem { span, .. } => *span,
            ParseError::ExpectedOpenParen { span, .. } => *span,
            ParseError::ExpectedType { span, .. } => *span,
        }
    }
    

}