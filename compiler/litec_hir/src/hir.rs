// hir.rs
use litec_span::{Span, StringId};
use litec_ast::token::{LiteralKind, TokenKind};

#[derive(Debug)]
pub struct Crate {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Function {
        visibility: Visibility,
        name: StringId,
        params: Vec<Param>,
        return_type: Option<Type>,
        body: Block,
        span: Span,
    },
    Struct {
        visibility: Visibility,
        name: StringId,
        fields: Vec<Field>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: StringId,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: StringId,
    pub ty: Type,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
    Named {
        name: StringId,
        span: Span,
    },
    // 可以添加更多类型变体
}

#[derive(Debug, Clone)]
pub enum Expr {
    // 基本表达式
    Literal {
        value: LiteralValue, // 解析后的值
        span: Span,
    },
    Ident {
        name: StringId,
        span: Span,
    },
    
    // 算术运算
    Addition {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Subtract {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Multiply {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Divide {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Remainder {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    
    // 比较运算
    Equal {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    NotEqual {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    LessThan {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    LessThanOrEqual {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    GreaterThan {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    GreaterThanOrEqual {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    
    // 逻辑运算
    LogicalAnd {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    LogicalOr {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    LogicalNot {
        expr: Box<Expr>,
        span: Span,
    },
    
    // 赋值运算 - 统一使用基本的 Assign 节点
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        span: Span,
        original_op: Option<TokenKind>, // 保留原始操作符信息
    },
    
    // 一元运算
    Negate {
        expr: Box<Expr>,
        span: Span,
    },
    Dereference {
        expr: Box<Expr>,
        span: Span,
    },
    AddressOf {
        expr: Box<Expr>,
        span: Span,
    },
    
    // 复合表达式
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Block {
        block: Block
    },
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Box<Expr>>,
        span: Span,
    },
    Loop {
        body: Box<Block>,
        span: Span,
    },
    Return {
        value: Option<Box<Expr>>,
        span: Span,
    },
    FieldAccess {
        base: Box<Expr>,
        field: StringId,
        span: Span,
    },
    PathAccess {
        base: Box<Expr>,
        path: StringId,
        span: Span,
    },
    
    // 临时变量（用于去糖过程中）
    Temp {
        id: u32,
        original_span: Span, // 原始表达式的span
        span: Span,          // 当前节点的span
    },
    
    // 分组表达式（保留括号语义）
    Grouped {
        expr: Box<Expr>,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Let {
        name: StringId,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        span: Span,
    },
    Return {
        value: Option<Box<Expr>>,
        span: Span,
    },
    Break {
        value: Option<Box<Expr>>,
        span: Span,
    },
    Continue {
        span: Span,
    },
    // 用于去糖的临时语句
    Temp {
        id: u32,
        value: Box<Expr>,
        original_span: Span, // 原始表达式的span
        span: Span,          // 当前语句的span
    },
}

#[derive(Debug, Clone)]
pub enum IntKind {
    I8, I16, I32, I64, I128, Isize,
    U8, U16, U32, U64, U128, Usize, // 添加无符号类型
    Unknown, 
}

#[derive(Debug, Clone)]
pub enum FloatKind {
    F32,
    F64,
    Unknown, // 用于未指定类型的浮点字面量
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Int {
        value: i128,        // 存储实际的整数值
        kind: IntKind,      // 整数类型信息
    },
    Float {
        value: f64,         // 存储实际的浮点数值
        kind: FloatKind,    // 浮点类型信息
    },
    Bool(bool),             // 布尔值
    Str(StringId),       // 字符串字面量（使用 StringId）
    Char(char),             // 字符字面量
}