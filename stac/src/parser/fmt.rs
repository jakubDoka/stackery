use crate::{BinaryAst, BlockAst, ExprAst, LitKindAst, OpCode, StructFieldAst, TokenKind, UnitAst};

pub fn format_ast(ast: &[ExprAst], ctx: &mut String, indent: usize) {
    for &ast in ast {
        format_expr(ast, ctx, indent);
        ctx.push_str(";\n");
    }
}

fn format_expr(ast: ExprAst, ctx: &mut String, indent: usize) {
    match ast {
        ExprAst::Unit(u) => format_unit(u, ctx, indent),
        ExprAst::Binary(&b) => format_binary(b, ctx, indent),
    }
}

fn format_binary(binary: BinaryAst, ctx: &mut String, indent: usize) {
    ctx.push('(');
    format_expr(binary.lhs, ctx, indent);
    ctx.push(' ');
    ctx.push_str(binary.op.kind.name());
    ctx.push(' ');
    format_expr(binary.rhs, ctx, indent);
    ctx.push(')');
}

fn indent_f(ctx: &mut String, indent: usize) {
    for _ in 0..indent {
        ctx.push_str("    ");
    }
}

fn format_list<T: Copy>(
    list: &[T],
    ctx: &mut String,
    indent: usize,
    [start, sep, end]: [TokenKind; 3],
    indented: bool,
    trailing: bool,
    mut fmt: impl FnMut(T, &mut String, usize),
) {
    ctx.push_str(start.name());
    let Some((last, list)) = list.split_last() else {
            ctx.push_str(end.name());
            return;
        };

    if indented {
        ctx.push('\n');
    }

    for &item in list {
        if indented {
            indent_f(ctx, indent + 1);
        }
        fmt(item, ctx, indent + indented as usize);
        ctx.push_str(sep.name());
        if indented {
            ctx.push('\n');
        } else {
            ctx.push(' ');
        }
    }

    if indented {
        indent_f(ctx, indent + 1);
    }
    fmt(*last, ctx, indent + indented as usize);
    if trailing {
        ctx.push_str(sep.name());
    }
    if indented {
        ctx.push('\n');
    }

    if indented {
        indent_f(ctx, indent);
    }
    ctx.push_str(end.name());
}

pub fn format_unit(unit: UnitAst, ctx: &mut String, indent: usize) {
    match unit {
        UnitAst::Literal(l) => match l.kind {
            LitKindAst::Int(i) => ctx.push_str(&i.value().to_string()),
            LitKindAst::Bool(b) => ctx.push_str(&b.to_string()),
            LitKindAst::Str(s) => ctx.push_str(s.as_str()),
        },
        UnitAst::Import(ident) | UnitAst::Ident(ident) => ctx.push_str(ident.ident.as_str()),
        UnitAst::Block(&b) => {
            format_block(b, ctx, indent);
        }
        UnitAst::Unary(u) => {
            ctx.push_str(u.op.kind.name());
            format_unit(u.expr, ctx, indent);
        }
        UnitAst::Array { elems, .. } => {
            format_list(
                elems,
                ctx,
                indent,
                [TokenKind::LBracket, TokenKind::Comma, TokenKind::RBracket],
                false,
                false,
                format_expr,
            );
        }
        UnitAst::FilledArray(a) => {
            ctx.push('[');
            format_expr(a.expr, ctx, indent);
            ctx.push(';');
            format_expr(a.len, ctx, indent);
            ctx.push(']');
        }
        UnitAst::Struct { fields, .. } => {
            format_list(
                fields,
                ctx,
                indent,
                [TokenKind::Struct, TokenKind::Comma, TokenKind::RBrace],
                true,
                true,
                |field, ctx, indent| match field {
                    StructFieldAst::Decl(named) => {
                        ctx.push_str(named.name.ident.as_str());
                        ctx.push_str(": ");
                        format_expr(named.expr, ctx, indent);
                    }
                    StructFieldAst::Inline(name) => {
                        ctx.push_str(name.ident.as_str());
                    }
                },
            );
        }
        UnitAst::Index(i) => {
            format_unit(i.expr, ctx, indent);
            ctx.push('[');
            format_expr(i.index, ctx, indent);
            ctx.push(']');
        }
        UnitAst::Enum(e) => {
            ctx.push_str(TokenKind::Enum.name());
            match e.value {
                Some(val) => {
                    ctx.push(' ');
                    ctx.push_str(e.name.ident.as_str());
                    ctx.push_str(": ");
                    format_expr(val, ctx, indent);
                    ctx.push_str(" }");
                }
                None => {
                    ctx.push_str(e.name.ident.as_str());
                    ctx.push('}');
                }
            }
        }
        UnitAst::Call(c) => {
            format_unit(c.caller, ctx, indent);
            format_list(
                c.args,
                ctx,
                indent,
                [TokenKind::LParen, TokenKind::Comma, TokenKind::RParen],
                false,
                false,
                format_expr,
            );
        }
        UnitAst::Func(f) => {
            format_list(
                f.args,
                ctx,
                indent,
                [
                    TokenKind::Op(OpCode::BitOr),
                    TokenKind::Comma,
                    TokenKind::Op(OpCode::BitOr),
                ],
                false,
                false,
                |param, ctx, indent| {
                    ctx.push_str(param.name.ident.as_str());
                    if let Some(default) = param.default {
                        ctx.push_str(": ");
                        format_unit(default, ctx, indent);
                    }
                },
            );
            ctx.push(' ');
            format_expr(f.body, ctx, indent);
        }
        UnitAst::Decl(d) => {
            ctx.push_str(d.name.ident.as_str());
            ctx.push_str(": ");
            format_expr(d.expr, ctx, indent);
        }
        UnitAst::Loop(l) => {
            ctx.push_str(TokenKind::Loop.name());
            if let Some(label) = l.label {
                ctx.push('.');
                ctx.push_str(label.ident.as_str());
            }
            ctx.push(' ');
            format_block(l.body, ctx, indent);
        }
        UnitAst::ForLoop(fl) => {
            ctx.push_str(TokenKind::For.name());
            if let Some(label) = fl.label {
                ctx.push('.');
                ctx.push_str(label.ident.as_str());
            }
            ctx.push(' ');
            ctx.push_str(fl.name.ident.as_str());
            ctx.push(' ');
            ctx.push_str(TokenKind::In.name());
            ctx.push(' ');
            format_expr(fl.iter, ctx, indent);
            ctx.push(' ');
            format_expr(fl.body, ctx, indent);
        }
        UnitAst::Break(_) => todo!(),
        UnitAst::Continue(_) => todo!(),
        UnitAst::Field(fa) => {
            format_unit(fa.expr, ctx, indent);
            ctx.push('.');
            ctx.push_str(fa.name.ident.as_str());
        }
        UnitAst::If(i) => {
            ctx.push_str(TokenKind::If.name());
            ctx.push(' ');
            format_expr(i.cond, ctx, indent);
            ctx.push(' ');
            format_block(i.then, ctx, indent);
            if let Some(else_) = i.else_ {
                ctx.push(' ');
                ctx.push_str(TokenKind::Else.name());
                ctx.push(' ');
                format_block(else_, ctx, indent);
            }
        }
        UnitAst::Ret { value, .. } => {
            ctx.push_str(TokenKind::Ret.name());
            if let Some(&expr) = value {
                ctx.push(' ');
                format_expr(expr, ctx, indent);
            }
        }
        UnitAst::Paren(&p) => {
            format_expr(p, ctx, indent);
        }
        UnitAst::Unknown(..) => ctx.push_str(TokenKind::Unknown.name()),
        UnitAst::Self_(..) => ctx.push_str(TokenKind::Self_.name()),
    }
}

fn format_block(block: BlockAst, ctx: &mut String, indent: usize) {
    format_list(
        block.exprs,
        ctx,
        indent,
        [TokenKind::LBrace, TokenKind::Semi, TokenKind::RBrace],
        true,
        block.trailing_semi,
        format_expr,
    );
}
