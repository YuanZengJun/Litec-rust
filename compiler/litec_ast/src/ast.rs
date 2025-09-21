use litec_span::{Span, StringId};
use crate::token::{LiteralKind, TokenKind};

#[derive(Debug)]
pub struct Crate {
    pub items: Vec<Item>
}

impl Crate {
    pub fn new(statements: Vec<Item>) -> Self {
        Crate { items: statements }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub enum Item {
    Function {
        visibility: Visibility,
        name: StringId,
        return_type: Option<TypeAnnotation>,
        params: Vec<Param>,
        body: Block,
        span: Span
    },
    Struct {
        visibility: Visibility,
        name: StringId,
        fields: Vec<Field>,
        span: Span
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: StringId,
    pub ty: TypeAnnotation,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: StringId,
    pub ty: TypeAnnotation,
    pub visibility: Visibility,
    pub span: Span
}

#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    Ident {
        name: StringId,
        span: Span
    }
}

impl TypeAnnotation {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident { span, .. } => *span
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: TokenKind,
        right: Box<Expr>,
        span: Span
    },
    Unary {
        op: TokenKind,
        operand: Box<Expr>,
        span: Span,
    },
    Posifix {
        op: TokenKind,
        expr: Box<Expr>,
        span: Span
    },
    Literal {
        kind: LiteralKind,
        value: StringId,
        suffix: Option<StringId>,
        span: Span,
    },
    Ident {
        name: StringId,
        span: Span,
    },
    Grouped {
        expr: Box<Expr>,
        span: Span,
    },
    Assignment {
        target: Box<Expr>,
        op: TokenKind,
        value: Box<Expr>,
        span: Span,
    },
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
    While {
        condition: Box<Expr>,
        body: Block,
        span: Span,
    },
    For {
        variable: Box<Expr>,
        generator: Box<Expr>,
        body: Block,
        span: Span
    },
    Loop {
        body: Block,
        span: Span
    },
    MemberAccess {
        accessed: Box<Expr>,
        op: TokenKind,
        name: StringId,
        span: Span
    },
    Bool {
        value: bool,
        span: Span
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr {
        expr: Box<Expr>
    },
    Let {
        name: StringId,
        ty: Option<TypeAnnotation>,
        value: Option<Expr>,
        span: Span,
    },
    Return {
        value: Option<Expr>,
        span: Span,
    },
    Continue {
        span: Span,
    },
    Break {
        value: Option<Expr>,
        span: Span
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Literal { span, .. } => *span,
            Expr::Ident { span, .. } => *span,
            Expr::Grouped { span, .. } => *span,
            Expr::Assignment { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Block { block } => block.span,
            Expr::If { span, .. } => *span,
            Expr::While { span, ..} => *span,
            Expr::Loop { span, .. } => *span,
            Expr::For { span, .. } => *span,
            // Expr::Function { function } => function.span,
            Expr::Posifix { span, .. } => *span,
            Expr::MemberAccess { span, .. } => *span,
            Expr::Bool { span, .. } => *span
        }
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expr { expr } => expr.span(),
            Stmt::Let { span, ..} => *span,
            Stmt::Return { span, .. } => *span,
            Stmt::Continue { span } => *span,
            Stmt::Break { span, .. } => *span,
        }
    }
}