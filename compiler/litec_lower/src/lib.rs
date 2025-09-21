use litec_ast::{ast::{
    Block as AstBlock, Crate as AstCrate, Expr as AstExpr, Field as AstField, Item as AstItem,
    Param as AstParam, Stmt as AstStmt, TypeAnnotation, Visibility as AstVisibility,
}, token::{LiteralKind, TokenKind}};
use litec_hir::hir::{
    Block as HirBlock, Crate as HirCrate, Expr as HirExpr, Field as HirField, FloatKind, IntKind, Item as HirItem, LiteralValue, Param as HirParam, Stmt as HirStmt, Type as HirType, Visibility as HirVisibility
};
use litec_errors::ParseError;
use litec_span::{get_global_string, intern_global, Span};

type LowerResult<T> = Result<T, ParseError>;

pub struct Lower {
    tmp_counter: usize,
}

impl Lower {
    pub fn new() -> Self {
        Lower { tmp_counter: 0 }
    }

    pub fn lower_crate(&mut self, ast: AstCrate) -> Result<HirCrate, Vec<ParseError>> {
        let mut errors = Vec::new();
        let mut items = Vec::new();

        for item in ast.items {
            match self.lower_item(item) {
                Ok(item) => items.push(item),
                Err(err) => errors.push(err),
            }
        }

        if errors.is_empty() {
            Ok(HirCrate {items: items})
        } else {
            Err(errors)
        }
    }

    fn lower_item(&mut self, item: AstItem) -> LowerResult<HirItem> {
        match item {
            AstItem::Function {
                visibility,
                name,
                return_type,
                params,
                body,
                span,
            } => {
                let params = params
                    .into_iter()
                    .map(|p| self.lower_param(p))
                    .collect::<Result<Vec<_>, _>>()?;

                let return_type = return_type
                    .map(|t| self.lower_type(t))
                    .transpose()?;

                let visibility = self.lower_visibility(visibility);
                let body = self.lower_block(body)?;

                Ok(HirItem::Function {
                    visibility,
                    name,
                    params,
                    return_type,
                    body,
                    span,
                })
            }
            AstItem::Struct {
                visibility,
                name,
                fields,
                span,
            } => {
                let visibility = self.lower_visibility(visibility);
                let fields = fields
                    .into_iter()
                    .map(|f| self.lower_field(f))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(HirItem::Struct {
                    visibility,
                    name,
                    fields,
                    span,
                })
            }
        }
    }

    fn lower_block(&mut self, block: AstBlock) -> LowerResult<HirBlock> {
        let stmts = block
            .stmts
            .into_iter()
            .map(|s| self.lower_stmt(s))
            .collect::<Result<Vec<_>, _>>()?;

        let tail = if let Some(expr) = block.tail {
            Some(Box::new(self.lower_expr(*expr)?))
        } else {
            None
        };

        Ok(HirBlock {
            stmts,
            expr: tail,
            span: block.span,
        })
    }

    fn lower_field(&mut self, field: AstField) -> LowerResult<HirField> {
        let ty = self.lower_type(field.ty)?;
        let visibility = self.lower_visibility(field.visibility);

        Ok(HirField {
            name: field.name,
            ty,
            visibility,
            span: field.span,
        })
    }

    fn lower_expr(&mut self, expr: AstExpr) -> LowerResult<HirExpr> {
        match expr {
            AstExpr::Block { block } => {
                let block = self.lower_block(block)?;
                Ok(HirExpr::Block { block })
            }
            AstExpr::Binary { left, op, right, span } => {
                let left = Box::new(self.lower_expr(*left)?);
                let right = Box::new(self.lower_expr(*right)?);

                let expr = match op {
                    TokenKind::Plus => HirExpr::Addition { left, right, span },
                    TokenKind::Minus => HirExpr::Subtract { left, right, span },
                    TokenKind::Star => HirExpr::Multiply { left, right, span },
                    TokenKind::Slash => HirExpr::Divide { left, right, span },
                    TokenKind::Percent => HirExpr::Remainder { left, right, span },

                    TokenKind::EqEq => HirExpr::Equal { left, right, span },
                    TokenKind::NotEq => HirExpr::NotEqual { left, right, span },
                    TokenKind::Lt => HirExpr::LessThan { left, right, span },
                    TokenKind::LtEq => HirExpr::LessThanOrEqual { left, right, span },
                    TokenKind::Gt => HirExpr::GreaterThan { left, right, span },
                    TokenKind::GtEq => HirExpr::GreaterThanOrEqual { left, right, span },

                    TokenKind::And => HirExpr::LogicalAnd { left, right, span },
                    TokenKind::Or => HirExpr::LogicalOr { left, right, span },

                    TokenKind::PlusEq => {
                        let target = left.clone();
                        let value = HirExpr::Addition { left: left, right, span };
                        return Ok(HirExpr::Assign {
                            target,
                            value: Box::new(value),
                            span,
                            original_op: Some(op),
                        });
                    }
                    TokenKind::MinusEq => {
                        let target = left.clone();
                        let value = HirExpr::Subtract { left: left, right, span };
                        return Ok(HirExpr::Assign {
                            target,
                            value: Box::new(value),
                            span,
                            original_op: Some(op),
                        });
                    }
                    TokenKind::StarEq => {
                        let target = left.clone();
                        let value = HirExpr::Multiply { left: left, right, span };
                        return Ok(HirExpr::Assign {
                            target,
                            value: Box::new(value),
                            span,
                            original_op: Some(op),
                        });
                    }
                    TokenKind::SlashEq => {
                        let target = left.clone();
                        let value = HirExpr::Divide { left: left, right, span };
                        return Ok(HirExpr::Assign {
                            target,
                            value: Box::new(value),
                            span,
                            original_op: Some(op),
                        });
                    }
                    TokenKind::PercentEq => {
                        let target = left.clone();
                        let value = HirExpr::Remainder { left: left, right, span };
                        return Ok(HirExpr::Assign {
                            target,
                            value: Box::new(value),
                            span,
                            original_op: Some(op),
                        });
                    }

                    _ => {
                        return Err(ParseError::InvalidOperatorTypes {
                            op: op,
                            span,
                        });
                    }
                };

                Ok(expr)
            }
            AstExpr::Unary { op, operand, span } => {
                let operand = Box::new(self.lower_expr(*operand)?);

                let expr = match op {
                    TokenKind::Bang => HirExpr::LogicalNot { expr: operand, span },
                    TokenKind::Minus => HirExpr::Negate { expr: operand, span },
                    _ => {
                        return Err(ParseError::InvalidOperatorTypes {
                            op,
                            span,
                        });
                    }
                };

                Ok(expr)
            }
            AstExpr::Posifix { op, expr, span } => {
                let operand = Box::new(self.lower_expr(*expr)?);

                let value = match op {
                    TokenKind::PlusPlus => {
                        let one = HirExpr::Literal {
                            value: LiteralValue::Int {
                                value: 1,
                                kind: IntKind::Unknown,
                            },
                            span: Span::new(0, 0),
                        };
                        HirExpr::Addition {
                            left: operand.clone(),
                            right: Box::new(one),
                            span,
                        }
                    }
                    TokenKind::MinusMinus => {
                        let one = HirExpr::Literal {
                            value: LiteralValue::Int {
                                value: 1,
                                kind: IntKind::Unknown,
                            },
                            span: Span::new(0, 0),
                        };
                        HirExpr::Subtract {
                            left: operand.clone(),
                            right: Box::new(one),
                            span,
                        }
                    }
                    _ => {
                        return Err(ParseError::InvalidOperatorTypes {
                            op,
                            span,
                        });
                    }
                };

                Ok(HirExpr::Assign {
                    target: operand,
                    value: Box::new(value),
                    span,
                    original_op: Some(op),
                })
            }
            AstExpr::Literal {
                kind: LiteralKind::Int { base },
                value,
                suffix,
                span,
            } => {
                let s = get_global_string(value).unwrap();

                let radix = base as u32;
                match i128::from_str_radix(&s, radix) {
                    Ok(num) => {
                        let int_kind = if let Some(suffix_sid) = suffix {
                            let suffix_str = get_global_string(suffix_sid).unwrap();
                            match suffix_str.as_ref() {
                                "i8" => IntKind::I8,
                                "i16" => IntKind::I16,
                                "i32" => IntKind::I32,
                                "i64" => IntKind::I64,
                                "i128" => IntKind::I128,
                                "isize" => IntKind::Isize,
                                "u8" => IntKind::U8,
                                "u16" => IntKind::U16,
                                "u32" => IntKind::U32,
                                "u64" => IntKind::U64,
                                "u128" => IntKind::U128,
                                "usize" => IntKind::Usize,
                                _ => {
                                    return Err(ParseError::InvalidLiteralSuffix {
                                        suffix: suffix_str.as_ref().to_string(),
                                        span,
                                    });
                                }
                            }
                        } else {
                            IntKind::Unknown
                        };

                        if !self.is_value_in_range(num, int_kind.clone()) {
                            return Err(ParseError::IntegerLiteralOutOfRange {
                                value: s.to_string(),
                                ty: format!("{:?}", int_kind),
                                span,
                            });
                        }

                        Ok(HirExpr::Literal {
                            value: LiteralValue::Int {
                                value: num,
                                kind: int_kind,
                            },
                            span,
                        })
                    }
                    Err(e) => Err(ParseError::IntegerLiteralParseError {
                        literal: s.to_string(),
                        base: radix,
                        error: e.to_string(),
                        span,
                    }),
                }
            }
            // 在 lower 函数中添加浮点数字面量处理
            AstExpr::Literal {
                kind: LiteralKind::Float { base },
                value,
                suffix,
                span,
            } => {
                let s = get_global_string(value).unwrap();
            
                // 解析为 f64（浮点数默认使用 f64 类型）
                let num = match s.parse::<f64>() {
                    Ok(num) => num,
                    Err(e) => {
                        return Err(ParseError::FloatLiteralParseError {
                            literal: s.to_string(),
                            error: e.to_string(),
                            span,
                        });
                    }
                };
            
                // 确定目标浮点类型
                let float_kind = if let Some(suffix_sid) = suffix {
                    let suffix_str = get_global_string(suffix_sid).unwrap();
                    match suffix_str.as_ref() {
                        "f32" => FloatKind::F32,
                        "f64" => FloatKind::F64,
                        _ => {
                            return Err(ParseError::InvalidFloatSuffix {
                                suffix: suffix_str.as_ref().to_string(),
                                span,
                            });
                        }
                    }
                } else {
                    FloatKind::Unknown // 默认类型
                };
            
                // 检查数值是否在目标类型范围内
                match float_kind {
                    FloatKind::F32 => {
                        if num < f32::MIN as f64 || num > f32::MAX as f64 {
                            return Err(ParseError::FloatLiteralOutOfRange {
                                value: s.to_string(),
                                ty: "f32".to_string(),
                                span,
                            });
                        }
                    }
                    FloatKind::F64 | _ => {
                        // f64 范围很大，通常不会超出，但保留检查
                        if num < f64::MIN || num > f64::MAX {
                            return Err(ParseError::FloatLiteralOutOfRange {
                                value: s.to_string(),
                                ty: "f64".to_string(),
                                span,
                            });
                        }
                    }
                }
            
                // 构建 HIR 浮点数字面量
                Ok(HirExpr::Literal {
                    value: LiteralValue::Float {
                        value: num,
                        kind: float_kind,
                    },
                    span,
                })
            }
            AstExpr::Literal { 
                kind: LiteralKind::Str { terminated }, 
                value, 
                suffix, 
                span 
            } => {
                let s = get_global_string(value).unwrap();
                if s.len() >= 2 && s.starts_with('"') && s.ends_with('"') {
                    let inner = &s[1..s.len()-1];
                    Ok(HirExpr::Literal { 
                        value: LiteralValue::Str(intern_global(inner)), 
                        span 
                    })
                } else {
                    Err(ParseError::InvalidStringLiteral {
                        literal: s.as_ref().to_string(),
                        span,
                    })
                }
            }

            // 字符字面量的正确处理（如果需要）
            AstExpr::Literal { 
                kind: LiteralKind::Char { terminated }, 
                value, 
                suffix, 
                span 
            } => {
                let s = get_global_string(value).unwrap();
                if s.len() == 3 && s.starts_with('\'') && s.ends_with('\'') {
                    let c = s.chars().nth(1).unwrap();
                    Ok(HirExpr::Literal { 
                        value: LiteralValue::Char(c), 
                        span 
                    })
                } else {
                    Err(ParseError::InvalidCharLiteral {
                        literal: s.as_ref().to_string(),
                        span,
                    })
                }
            }
            AstExpr::Ident { name, span } => Ok(HirExpr::Ident { name, span }),
            AstExpr::Grouped { expr, span } => {
                let expr = self.lower_expr(*expr)?;
                Ok(HirExpr::Grouped { expr: Box::new(expr), span })
            }
            AstExpr::Assignment { target, op, value, span } => {
                let target = Box::new(self.lower_expr(*target)?);
                let value = Box::new(self.lower_expr(*value)?);

                Ok(HirExpr::Assign {
                    target,
                    value,
                    span,
                    original_op: Some(op),
                })
            }
            AstExpr::Call { callee, args, span } => {
                let callee = Box::new(self.lower_expr(*callee)?);
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(HirExpr::Call {
                    callee,
                    args,
                    span,
                })
            }
            AstExpr::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let condition = Box::new(self.lower_expr(*condition)?);
                let then_branch = self.lower_block(then_branch)?;
                let else_branch = if else_branch.is_some() {
                    Some(Box::new(self.lower_expr(*else_branch.unwrap())?))
                } else {
                    None
                };

                Ok(HirExpr::If {
                    condition,
                    then_branch,
                    else_branch,
                    span,
                })
            }
            AstExpr::While { condition, body, span } => {
                let condition = Box::new(self.lower_expr(*condition)?);
                let body = self.lower_block(body)?;

                let body = {
                    HirBlock {
                        stmts: Vec::new(),
                        expr: Some(
                            Box::new(
                                HirExpr::If { 
                                    condition: condition, 
                                    then_branch: body, 
                                    else_branch: Some(
                                        Box::new(
                                            HirExpr::Block { 
                                                block: HirBlock { 
                                                    stmts: vec![
                                                        HirStmt::Break { 
                                                            value: None, 
                                                            span: span 
                                                        }
                                                    ], 
                                                    expr: None, 
                                                    span
                                                } 
                                            }
                                        )
                                    ), 
                                    span: span 
                                }
                            )
                        ),
                        span: span
                    }
                };

                Ok(HirExpr::Loop {
                    body: Box::new(body),
                    span: span,
                })
            }
            AstExpr::For {
                variable,
                generator,
                body,
                span,
            } => {
                panic!("之后实现")
            }
            AstExpr::Loop { body, span } => {
                let body = self.lower_block(body)?;
                Ok(HirExpr::Loop { body: Box::new(body), span })
            }
            AstExpr::MemberAccess { accessed, op, name, span } => {
                let accessed = Box::new(self.lower_expr(*accessed)?);
                match op {
                    TokenKind::Dot => Ok(HirExpr::FieldAccess { base: accessed, field: name, span: span }),
                    TokenKind::PathAccess => Ok(HirExpr::PathAccess { base: accessed, path: name, span: span }),

                    _ => {
                        panic!("未知op {:?}", op);
                    }
                }
            }
            AstExpr::Bool { value, span } => Ok(HirExpr::Literal {
                value: LiteralValue::Bool(value),
                span,
            }),
        }
    }

    fn lower_stmt(&mut self, stmt: AstStmt) -> LowerResult<HirStmt> {
        match stmt {
            AstStmt::Expr { expr } => {
                let expr = self.lower_expr(*expr)?;
                Ok(HirStmt::Expr(Box::new(expr)))
            }
            AstStmt::Let { name, ty, value, span } => {
                let ty = ty.map(|t| self.lower_type(t)).transpose()?;
                let value = value
                    .map(|v| self.lower_expr(v))
                    .transpose()?
                    .map(Box::new);

                Ok(HirStmt::Let {
                    name,
                    ty,
                    value,
                    span,
                })
            }
            AstStmt::Return { value, span } => {
                let value = value
                    .map(|v| self.lower_expr(v))
                    .transpose()?
                    .map(Box::new);

                Ok(HirStmt::Return { value, span })
            }
            AstStmt::Continue { span } => Ok(HirStmt::Continue { span }),
            AstStmt::Break { value, span } => {
                let value = value
                    .map(|v| self.lower_expr(v))
                    .transpose()?
                    .map(Box::new);

                Ok(HirStmt::Break { value, span })
            }
        }
    }

    fn lower_param(&mut self, param: AstParam) -> LowerResult<HirParam> {
        let ty = self.lower_type(param.ty)?;
        Ok(HirParam {
            name: param.name,
            ty,
            span: param.span,
        })
    }

    fn lower_visibility(&mut self, visibility: AstVisibility) -> HirVisibility {
        match visibility {
            AstVisibility::Public => HirVisibility::Public,
            AstVisibility::Private => HirVisibility::Private,
        }
    }

    fn lower_type(&mut self, ty: TypeAnnotation) -> LowerResult<HirType> {
        match ty {
            TypeAnnotation::Ident { name, span } => Ok(HirType::Named { name, span }),
        }
    }

    // 检查值是否在目标整数类型范围内
    fn is_value_in_range(&self, value: i128, kind: IntKind) -> bool {
        match kind {
            IntKind::I8 => value >= i8::MIN as i128 && value <= i8::MAX as i128,
            IntKind::I16 => value >= i16::MIN as i128 && value <= i16::MAX as i128,
            IntKind::I32 => value >= i32::MIN as i128 && value <= i32::MAX as i128,
            IntKind::I64 => value >= i64::MIN as i128 && value <= i64::MAX as i128,
            IntKind::I128 => true,
            IntKind::Isize => {
                if cfg!(target_pointer_width = "64") {
                    value >= i64::MIN as i128 && value <= i64::MAX as i128
                } else if cfg!(target_pointer_width = "32") {
                    value >= i32::MIN as i128 && value <= i32::MAX as i128
                } else {
                    true
                }
            }
            IntKind::U8 => value >= 0 && value <= u8::MAX as i128,
            IntKind::U16 => value >= 0 && value <= u16::MAX as i128,
            IntKind::U32 => value >= 0 && value <= u32::MAX as i128,
            IntKind::U64 => value >= 0 && value <= u64::MAX as i128,
            IntKind::U128 => value >= 0,
            IntKind::Usize => {
                if cfg!(target_pointer_width = "64") {
                    value >= 0 && value <= u64::MAX as i128
                } else if cfg!(target_pointer_width = "32") {
                    value >= 0 && value <= u32::MAX as i128
                } else {
                    value >= 0
                }
            }
            IntKind::Unknown => true,
        }
    }
}