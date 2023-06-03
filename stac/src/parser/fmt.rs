use crate::*;
use mini_alloc::*;

pub fn format_ast(ast: &[ExprAst], ctx: &mut String, indent: usize, interner: &StrInterner) {
    for &ast in ast {
        format_expr(ast, ctx, indent, interner);
        ctx.push_str(";\n");
    }
}

fn format_expr(ast: ExprAst, ctx: &mut String, indent: usize, interner: &StrInterner) {
    match ast {
        ExprAst::Unit(u) => format_unit(u, ctx, indent, interner),
        ExprAst::Binary(&b) => format_binary(b, ctx, indent, interner),
    }
}

fn format_binary(binary: BinaryAst, ctx: &mut String, indent: usize, interner: &StrInterner) {
    ctx.push('(');
    format_expr(binary.lhs, ctx, indent, interner);
    ctx.push(' ');
    ctx.push_str(binary.op.kind.name());
    ctx.push(' ');
    format_expr(binary.rhs, ctx, indent, interner);
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
    interner: &StrInterner,
    [start, sep, end]: [TokenKind; 3],
    indented: bool,
    trailing: bool,
    mut fmt: impl FnMut(T, &mut String, usize, &StrInterner),
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
        fmt(item, ctx, indent + indented as usize, interner);
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
    fmt(*last, ctx, indent + indented as usize, interner);
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

pub fn format_unit(unit: UnitAst, ctx: &mut String, indent: usize, interner: &StrInterner) {
    match unit {
        UnitAst::Literal(l) => match l.kind {
            LiteralKindAst::Int(i) => ctx.push_str(&u64::from_ne_bytes(i).to_string()),
            LiteralKindAst::Bool(b) => ctx.push_str(&b.to_string()),
            LiteralKindAst::Str(s) => ctx.push_str(&interner[s]),
        },
        UnitAst::Import(ident) | UnitAst::Ident(ident) => ctx.push_str(&interner[ident.ident]),
        UnitAst::Block(b) => {
            format_list(
                b.exprs,
                ctx,
                indent,
                interner,
                [TokenKind::LBrace, TokenKind::Semi, TokenKind::RBrace],
                true,
                b.trailing_semi,
                format_expr,
            );
        }
        UnitAst::Unary(u) => {
            ctx.push_str(u.op.kind.name());
            format_unit(u.expr, ctx, indent, interner);
        }
        UnitAst::Array { elems, .. } => {
            format_list(
                elems,
                ctx,
                indent,
                interner,
                [TokenKind::LBracket, TokenKind::Comma, TokenKind::RBracket],
                false,
                false,
                format_expr,
            );
        }
        UnitAst::FilledArray(a) => {
            ctx.push('[');
            format_expr(a.expr, ctx, indent, interner);
            ctx.push(';');
            format_expr(a.len, ctx, indent, interner);
            ctx.push(']');
        }
        UnitAst::Tuple { values, .. } => {
            format_list(
                values,
                ctx,
                indent,
                interner,
                [TokenKind::Tuple, TokenKind::Comma, TokenKind::RParen],
                false,
                false,
                format_expr,
            );
        }
        UnitAst::Struct { fields, .. } => {
            format_list(
                fields,
                ctx,
                indent,
                interner,
                [TokenKind::Struct, TokenKind::Comma, TokenKind::RBrace],
                true,
                true,
                |field, ctx, indent, interner| match field {
                    StructFieldAst::Decl(named) => {
                        ctx.push_str(&interner[named.name.ident]);
                        ctx.push_str(": ");
                        format_expr(named.expr, ctx, indent, interner);
                    }
                    StructFieldAst::Inline(name) => {
                        ctx.push_str(&interner[name.ident]);
                    }
                    StructFieldAst::Embed(expr) => {
                        ctx.push_str(TokenKind::DoubleDot.name());
                        format_expr(expr, ctx, indent, interner);
                    }
                },
            );
        }
        UnitAst::Index(i) => {
            format_unit(i.expr, ctx, indent, interner);
            ctx.push('[');
            format_expr(i.index, ctx, indent, interner);
            ctx.push(']');
        }
        UnitAst::Enum(e) => {
            ctx.push_str(TokenKind::Enum.name());
            match e.value {
                Some(val) => {
                    ctx.push(' ');
                    ctx.push_str(&interner[e.name.ident]);
                    ctx.push_str(": ");
                    format_expr(val, ctx, indent, interner);
                    ctx.push_str(" }");
                }
                None => {
                    ctx.push_str(&interner[e.name.ident]);
                    ctx.push('}');
                }
            }
        }
        UnitAst::Call(c) => {
            format_unit(c.caller, ctx, indent, interner);
            format_list(
                c.args,
                ctx,
                indent,
                interner,
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
                interner,
                [
                    TokenKind::Op(OpCode::BitOr),
                    TokenKind::Comma,
                    TokenKind::Op(OpCode::BitOr),
                ],
                false,
                false,
                |param, ctx, indent, interner| {
                    ctx.push_str(&interner[param.name.ident]);
                    if let Some(default) = param.default {
                        ctx.push_str(": ");
                        format_unit(default, ctx, indent, interner);
                    }
                },
            );
            ctx.push(' ');
            format_expr(f.body, ctx, indent, interner);
        }
        UnitAst::Decl(d) => {
            ctx.push_str(&interner[d.name.ident]);
            ctx.push_str(": ");
            format_expr(d.expr, ctx, indent, interner);
        }
        UnitAst::Loop(l) => {
            ctx.push_str(TokenKind::Loop.name());
            if let Some(label) = l.label {
                ctx.push('.');
                ctx.push_str(&interner[label.ident]);
            }
            ctx.push(' ');
            format_expr(l.body, ctx, indent, interner);
        }
        UnitAst::ForLoop(fl) => {
            ctx.push_str(TokenKind::For.name());
            if let Some(label) = fl.label {
                ctx.push('.');
                ctx.push_str(&interner[label.ident]);
            }
            ctx.push(' ');
            ctx.push_str(&interner[fl.var.ident]);
            ctx.push(' ');
            ctx.push_str(TokenKind::In.name());
            ctx.push(' ');
            format_expr(fl.iter, ctx, indent, interner);
            ctx.push(' ');
            format_expr(fl.body, ctx, indent, interner);
        }
        UnitAst::Break(_) => todo!(),
        UnitAst::Continue(_) => todo!(),
        UnitAst::Field(fa) => {
            format_unit(fa.expr, ctx, indent, interner);
            ctx.push('.');
            ctx.push_str(&interner[fa.name.ident]);
        }
        UnitAst::If(i) => {
            ctx.push_str(TokenKind::If.name());
            ctx.push(' ');
            format_expr(i.cond, ctx, indent, interner);
            ctx.push(' ');
            format_expr(i.then, ctx, indent, interner);
            if let Some(else_) = i.else_ {
                ctx.push(' ');
                ctx.push_str(TokenKind::Else.name());
                ctx.push(' ');
                format_expr(else_, ctx, indent, interner);
            }
        }
        UnitAst::Ret { value, .. } => {
            ctx.push_str(TokenKind::Ret.name());
            if let Some(&expr) = value {
                ctx.push(' ');
                format_expr(expr, ctx, indent, interner);
            }
        }
        UnitAst::Paren(&p) => {
            format_expr(p, ctx, indent, interner);
        }
        UnitAst::Unknown(..) => ctx.push_str(TokenKind::Unknown.name()),
    }
}
