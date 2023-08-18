use crate::{BinaryAst, BlockAst, ExprAst, LitKindAst, ModItemAst, RecordKind, TokenKind, UnitAst};

pub fn format_ast(ast: &[ModItemAst], ctx: &mut String, indent: usize) {
    for ast in ast {
        match ast {
            ModItemAst::Decl(l) => format_unit(&UnitAst::Decl(&l), ctx, indent),
        }
        ctx.push_str(";\n");
    }
}

fn format_expr(ast: &ExprAst, ctx: &mut String, indent: usize) {
    match ast {
        ExprAst::Unit(u) => format_unit(u, ctx, indent),
        ExprAst::Binary(b) => format_binary(b, ctx, indent),
    }
}

fn format_binary(binary: &BinaryAst, ctx: &mut String, indent: usize) {
    ctx.push('(');
    format_expr(&binary.lhs, ctx, indent);
    ctx.push(' ');
    ctx.push_str(binary.op.kind.name());
    ctx.push(' ');
    format_expr(&binary.rhs, ctx, indent);
    ctx.push(')');
}

fn indent_f(ctx: &mut String, indent: usize) {
    for _ in 0..indent {
        ctx.push_str("    ");
    }
}

fn format_list<T>(
    list: &[T],
    ctx: &mut String,
    indent: usize,
    [start, sep, end]: [TokenKind; 3],
    indented: bool,
    trailing: bool,
    mut fmt: impl FnMut(&T, &mut String, usize),
) {
    ctx.push_str(start.name());
    let Some((last, list)) = list.split_last() else {
        ctx.push_str(end.name());
        return;
    };

    if indented {
        ctx.push('\n');
    }

    for item in list {
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
    fmt(last, ctx, indent + indented as usize);
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

pub fn format_unit(unit: &UnitAst, ctx: &mut String, indent: usize) {
    match unit {
        UnitAst::Literal(ref l) => match l.kind {
            LitKindAst::Int(i) => ctx.push_str(&i.value().to_string()),
            LitKindAst::Bool(b) => ctx.push_str(&b.to_string()),
        },
        UnitAst::Ident(ident) => ctx.push_str(ident.ident.as_str()),
        UnitAst::Import(ident) => ctx.push_str(ident.ident.as_str()),
        UnitAst::If(i) => {
            ctx.push_str("if ");
            format_expr(&i.cond, ctx, indent);
            ctx.push_str(" ");
            format_expr(&i.then, ctx, indent);
            if let Some(else_) = &i.else_ {
                ctx.push_str(" ");
                ctx.push_str("else ");
                format_expr(else_, ctx, indent);
            }
        }
        UnitAst::Block(b) => {
            format_block(b, ctx, indent);
        }
        UnitAst::Unary(u) => {
            ctx.push_str(u.op.kind.name());
            format_unit(&u.expr, ctx, indent);
        }
        UnitAst::Call(c) => {
            format_unit(&c.caller, ctx, indent);
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
            ctx.push_str("fn");
            format_list(
                f.args,
                ctx,
                indent,
                [TokenKind::LParen, TokenKind::Comma, TokenKind::RParen],
                false,
                false,
                |param, ctx, indent| {
                    ctx.push_str(param.name.ident.as_str());
                    ctx.push_str(": ");
                    format_expr(&param.ty, ctx, indent);
                },
            );

            if let Some(ty) = &f.return_ty {
                ctx.push_str(": ");
                format_expr(ty, ctx, indent);
            }

            ctx.push(' ');
            format_expr(&f.body, ctx, indent);
        }
        UnitAst::Decl(d) => {
            ctx.push_str("let ");
            ctx.push_str(d.name.ident.as_str());
            if let Some(ty) = &d.ty {
                ctx.push_str(": ");
                format_expr(ty, ctx, indent);
            }
            if let Some(expr) = &d.value {
                ctx.push_str(" = ");
                format_expr(expr, ctx, indent);
            }
        }
        UnitAst::Paren(p) => {
            format_expr(p, ctx, indent);
        }
        UnitAst::Field(f) => {
            format_unit(&f.on, ctx, indent);
            ctx.push('.');
            ctx.push_str(f.name.ident.as_str());
        }
        UnitAst::Rt(e) => {
            ctx.push_str("rt ");
            format_expr(e, ctx, indent);
        }
        UnitAst::Return(e) => {
            ctx.push_str("return ");
            if let Some(ref e) = e.expr {
                format_expr(e, ctx, indent);
            }
        }
        UnitAst::Record(r) => {
            match r.kind {
                RecordKind::Prod => ctx.push_str("struct "),
                RecordKind::Sum => ctx.push_str("enum "),
                RecordKind::Max => ctx.push_str("union "),
            };

            format_list(
                r.fields,
                ctx,
                indent,
                [TokenKind::LBrace, TokenKind::Comma, TokenKind::RBrace],
                true,
                false,
                |field, ctx, indent| {
                    ctx.push_str(field.name.ident.as_str());
                    ctx.push_str(": ");
                    format_expr(&field.value, ctx, indent);
                },
            );
        }
        UnitAst::Ctor(c) => {
            format_unit(&c.ty, ctx, indent);
            ctx.push_str(".");
            format_list(
                c.fields,
                ctx,
                indent,
                [TokenKind::LBrace, TokenKind::Comma, TokenKind::RBrace],
                true,
                false,
                |field, ctx, indent| {
                    ctx.push_str(field.name.ident.as_str());
                    if let Some(value) = &field.value {
                        ctx.push_str(": ");
                        format_expr(value, ctx, indent);
                    }
                },
            );
        }
    }
}

fn format_block(block: &BlockAst, ctx: &mut String, indent: usize) {
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
