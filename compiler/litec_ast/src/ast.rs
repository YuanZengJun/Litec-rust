use std::fmt;
use litec_span::{Span, StringId, get_global_string};
use crate::token::{LiteralKind, TokenKind};

pub struct Crate {
    statements: Vec<Item>
}

impl Crate {
    pub fn new(statements: Vec<Item>) -> Self {
        Crate { statements: statements }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Flag {
    Public,
    Private,
}

#[derive(Debug)]
pub enum Item {
    Function(Function),
    Struct {
        name: StringId,
        fields: Vec<Field>,
        span: Span
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: StringId,
    pub return_type: Option<TypeAnnotation>,
    pub params: Vec<Param>,
    pub body: Expr,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: StringId,
    pub r#type: TypeAnnotation,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: StringId, // 改为 StringId 而不是 Expr
    pub r#type: TypeAnnotation,
    pub flag: Flag,
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

#[derive(Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: TokenKind,
        right: Box<Expr>,
        span: Span
    },
    Unary {
        op: TokenKind,
        expr: Box<Expr>,
        span: Span,
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
        stmts: Vec<Stmt>,
        tail: Option<Box<Expr>>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
        span: Span,
    },
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },
    For {
        variable: Box<Expr>,
        generator: Box<Expr>,
        body: Box<Expr>,
        span: Span
    },
    Loop {
        body: Box<Expr>,
        span: Span
    },
    Function {
        function: Box<Function>
    }
}

#[derive(Clone)]
pub enum Stmt {
    Expr {
        expr: Box<Expr>,
        span: Span,
    },
    Let {
        name: StringId,
        r#type: Option<TypeAnnotation>,
        value: Option<Expr>,
        span: Span,
    },
    Return {
        value: Option<Expr>,
        span: Span,
    },
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
            Expr::Block { span, .. } => *span,
            Expr::If { span, .. } => *span,
            Expr::While { span, ..} => *span,
            Expr::Loop { span, .. } => *span,
            Expr::For { span, .. } => *span,
            Expr::Function { function } => function.span,
        }
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expr { span, .. } => *span,
            Stmt::Let { span, ..} => *span,
            Stmt::Return { span, .. } => *span,
        }
    }
}

// Helper function to resolve StringId to string
fn resolve_string(id: StringId) -> String {
    get_global_string(id).unwrap_or_else(|| format!("<unknown:{}>", id.0).into()).to_string()
}

// Implement Debug for Expr with pretty printing support
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            // Pretty-print mode (# modifier)
            match self {
                Expr::Binary { left, op, right, span } => {
                    f.debug_struct("Binary")
                        .field("op", op)
                        .field("left", &left)
                        .field("right", &right)
                        .field("span", span)
                        .finish()
                }
                Expr::Unary { op, expr, span } => {
                    f.debug_struct("Unary")
                        .field("op", op)
                        .field("expr", &expr)
                        .field("span", span)
                        .finish()
                }
                Expr::Literal { kind, value, suffix, span } => {
                    let value_str = resolve_string(*value);
                    let suffix_str = suffix.map(resolve_string);
                    
                    f.debug_struct("Literal")
                        .field("kind", kind)
                        .field("value", &value_str)
                        .field("suffix", &suffix_str)
                        .field("span", span)
                        .finish()
                }
                Expr::Ident { name, span } => {
                    let name_str = resolve_string(*name);
                    
                    f.debug_struct("Ident")
                        .field("name", &name_str)
                        .field("span", span)
                        .finish()
                }
                Expr::Grouped { expr, span } => {
                    f.debug_struct("Grouped")
                        .field("expr", &expr)
                        .field("span", span)
                        .finish()
                }
                Expr::Assignment { target, op, value, span } => {
                    f.debug_struct("Assignment")
                        .field("target", &target)
                        .field("op", op)
                        .field("value", &value)
                        .field("span", span)
                        .finish()
                }
                Expr::Call { callee, args, span } => {
                    f.debug_struct("Call")
                        .field("callee", &callee)
                        .field("args", &args)
                        .field("span", span)
                        .finish()
                }
                Expr::Block { stmts, tail, span } => {
                    f.debug_struct("Block")
                        .field("stmts", &stmts)
                        .field("tail", &tail)
                        .field("span", span)
                        .finish()
                }
                Expr::If { condition, then_branch, else_branch, span } => {
                    f.debug_struct("If")
                        .field("condition", &condition)
                        .field("then_branch", &then_branch)
                        .field("else_branch", &else_branch)
                        .field("span", span)
                        .finish()
                }
                Expr::While { condition, body, span } => {
                    f.debug_struct("While")
                        .field("condition", &condition)
                        .field("body", &body)
                        .field("span", span)
                        .finish()
                }
                Expr::Loop { body, span } => {
                    f.debug_struct("Loop")
                        .field("body", &body)
                        .field("span", &span)
                        .finish()
                }
                Expr::For { variable, generator, body, span } => {
                    f.debug_struct("For")
                        .field("variable", &variable)
                        .field("generator", &generator)
                        .field("body", &body)
                        .field("span", span)
                        .finish()
                }
                Expr::Function { function, ..} => {
                    let name_str = resolve_string(function.name);
                    
                    // 修复这里：直接使用 TypeAnnotation 而不是尝试转换为 Expr
                    let params_debug: Vec<(String, &TypeAnnotation)> = function.params.iter()
                        .map(|param| (resolve_string(param.name), &param.r#type))
                        .collect();
                    
                    f.debug_struct("Function")
                        .field("name", &name_str)
                        .field("params", &params_debug)
                        .field("return_type", &function.return_type)
                        .field("body", &function.body)
                        .field("span", &function.span)
                        .finish()
                }
            }
        } else {
            // Compact mode
            match self {
                Expr::Binary { left, op, right, .. } => {
                    write!(f, "Binary({:?} {:?} {:?})", op, left, right)
                }
                Expr::Unary { op, expr, .. } => {
                    write!(f, "Unary({:?} {:?})", op, expr)
                }
                Expr::Literal { kind, value, suffix, .. } => {
                    let value_str = resolve_string(*value);
                    if let Some(s) = suffix {
                        let suffix_str = resolve_string(*s);
                        write!(f, "Literal({:?}, {}, {})", kind, value_str, suffix_str)
                    } else {
                        write!(f, "Literal({:?}, {})", kind, value_str)
                    }
                }
                Expr::Ident { name, .. } => {
                    let name_str = resolve_string(*name);
                    write!(f, "Ident({})", name_str)
                }
                Expr::Grouped { expr, .. } => {
                    write!(f, "Grouped({:?})", expr)
                }
                Expr::Assignment { target, op, value, .. } => {
                    write!(f, "Assignment({:?} {:?} {:?})", op, target, value)
                }
                Expr::Call { callee, args, .. } => {
                    write!(f, "Call({:?}, {:?})", callee, args)
                }
                Expr::Block { stmts, tail, .. } => {
                    write!(f, "Block({:?}, {:?})", stmts, tail)
                }
                Expr::If { condition, then_branch, else_branch, .. } => {
                    if let Some(else_expr) = else_branch {
                        write!(f, "If({:?}, {:?}, {:?})", condition, then_branch, else_expr)
                    } else {
                        write!(f, "If({:?}, {:?})", condition, then_branch)
                    }
                }
                Expr::While { condition, body, .. } => {
                    write!(f, "While({:?}, {:?})", condition, body)
                }
                Expr::Loop { body, .. } => {
                    write!(f, "Loop({:?})", body)
                }
                Expr::For { variable, generator, body, .. } => {
                    write!(f, "For({:?}, {:?}, {:?})", variable, generator, body)
                }
                Expr::Function { function } => {
                    let name_str = resolve_string(function.name);
                    let params_str = function.params.iter()
                        .map(|param| {
                            let name_str = resolve_string(param.name);
                            format!("{}: {:?}", name_str, param.r#type)
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    
                    if let Some(return_type) = &function.return_type {
                        write!(f, "Function({}, [{}], -> {:?}, {:?})", name_str, params_str, return_type, function.body)
                    } else {
                        write!(f, "Function({}, [{}], {:?})", name_str, params_str, function.body)
                    }
                }
            }
        }
    }
}

// Implement Debug for Stmt with pretty printing support
impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            // Pretty-print mode (# modifier)
            match self {
                Stmt::Expr { expr, span } => {
                    f.debug_struct("Expr")
                        .field("expr", &expr)
                        .field("span", span)
                        .finish()
                }
                Stmt::Let { name, r#type, value, span } => {
                    let name_str = resolve_string(*name);
                    
                    let mut debug = f.debug_struct("Let");
                    debug.field("name", &name_str);
                    
                    if let Some(r#type) = r#type {
                        debug.field("type", r#type);
                    }

                    if let Some(expr) = value {
                        debug.field("value", &expr);
                    }
                    
                    debug.field("span", span).finish()
                }
                Stmt::Return { value, span } => {
                    let mut debug = f.debug_struct("Return");
                    
                    if let Some(expr) = value {
                        debug.field("value", &expr);
                    }
                    
                    debug.field("span", span).finish()
                }
            }
        } else {
            // Compact mode
            match self {
                Stmt::Expr { expr, .. } => {
                    write!(f, "Expr({:?})", expr)
                }
                Stmt::Let { name, value, .. } => {
                    let name_str = resolve_string(*name);
                    if let Some(expr) = value {
                        write!(f, "Let({}, {:?})", name_str, expr)
                    } else {
                        write!(f, "Let({})", name_str)
                    }
                }
                Stmt::Return { value, .. } => {
                    if let Some(expr) = value {
                        write!(f, "Return({:?})", expr)
                    } else {
                        write!(f, "Return")
                    }
                }
            }
        }
    }
}

// Implement Debug for Crate
impl fmt::Debug for Crate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("Crate")
                .field("statements", &self.statements)
                .finish()
        } else {
            write!(f, "Crate({:?})", self.statements)
        }
    }
}