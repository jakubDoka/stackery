use std::{fmt, ops::Not, result};

use mini_alloc::{Diver, IdentStr};

use crate::{
    OpCode, Parser, RecordKind, Severty, Span, Token,
    TokenKind::{self, *},
    TransposeOpt,
};

impl<'ctx, 'src, 'arena, 'arena_ctx> Parser<'ctx, 'src, 'arena, 'arena_ctx> {
    const NON_EXPR_TOKENS: [TokenKind; 1] = [Semi];

    pub fn parse(mut self, mut diver: Diver) -> Option<&'arena [ModItemAst<'arena>]> {
        let mut items = diver.dive::<ModItemAst<'arena>>();

        loop {
            self.skip_semis();
            if self.peek().kind == Eof {
                break;
            }
            let item = self.mod_item(items.untyped_dive())?;
            items.push(item);
        }

        Some(self.arena.alloc_rev_iter(items))
    }

    pub fn skip_semis(&mut self) -> bool {
        let mut skipped = false;
        while self.peek().kind == Semi {
            self.next();
            skipped = true;
        }
        skipped
    }

    fn mod_item(&mut self, diver: Diver) -> Option<ModItemAst<'arena>> {
        let tok = self.next();
        match tok.kind {
            Let => self.decl(diver, tok.span).map(ModItemAst::Decl),
            _ => self.unex_tok(tok, "expected a declaration")?,
        }
    }

    pub(super) fn expr(&mut self, mut diver: Diver) -> Option<ExprAst<'arena>> {
        let unit = ExprAst::Unit(self.unit(diver.untyped_dive())?);

        self.binary(diver, unit, u8::MAX)
    }

    fn binary(
        &mut self,
        mut diver: Diver,
        mut lhs: ExprAst<'arena>,
        prev_prec: u8,
    ) -> Option<ExprAst<'arena>> {
        while let Token {
            kind: Op(kind),
            span,
            source
        } = self.peek() && kind.prec() < prev_prec
        {
            self.next();

            let mut rhs = ExprAst::Unit(self.unit(diver.untyped_dive())?);
            rhs = self.binary(diver.untyped_dive(), rhs, kind.prec())?;

            if kind == OpCode::Assign && let Some(kind) = OpCode::split_assign(source) {
                rhs = ExprAst::Binary(self.arena.alloc(BinaryAst {
                    lhs: lhs.clone(),
                    rhs,
                    op: OpAst { span, kind },
                }));
            }

            lhs = ExprAst::Binary(self.arena.alloc(BinaryAst {
                lhs,
                rhs,
                op: OpAst { span, kind },
            }));
        }

        Some(lhs)
    }

    fn unex_tok(&mut self, span: Token, message: &str) -> Option<!> {
        self.diags
            .builder(self.files)
            .footer(Severty::Error, message)
            .annotation(
                Severty::Error,
                span.span,
                format_args!("unexpected {}", span.kind),
            )
            .terminate()
    }

    fn unit(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let token = self.next();

        let expr = match token.kind {
            If => self.if_expr(diver.untyped_dive(), token.span),
            Else => self.unex_tok(token, "else can only follow an if expression")?,
            Fn => self.func(diver.untyped_dive(), token.span),
            Rt => self.rt(diver.untyped_dive(), token.span),
            Mut => self.unex_tok(token, "mut can only follow a declaration")?,
            Record(kind) => self.record(diver.untyped_dive(), kind, token.span),
            Comma => self.unex_tok(token, "comma can only follow an expression")?,
            Return => self.return_expr(diver.untyped_dive(), token.span),
            Semi => self.unex_tok(
                token,
                "semicolon can only follow an expression inside a block",
            )?,
            Colon => self.unex_tok(token, "colon can only follow pattern")?,
            LBrace => self.block(diver.untyped_dive(), token.span),
            RBrace => self.unex_tok(token, "unmatched right brace")?,
            LParen => self.paren(diver.untyped_dive()),
            RParen => self.unex_tok(token, "unmatched right parenthesis")?,
            Ident => Some(UnitAst::Ident(IdentAst {
                span: token.span,
                ident: IdentStr::from_str(token.source),
            })),
            Import => Some(UnitAst::Import(IdentAst {
                span: token.span,
                ident: IdentStr::from_str(
                    &token.source[":{".len()..token.source.len() - "}".len()],
                ),
            })),
            Let => self
                .decl(diver.untyped_dive(), token.span)
                .map(|d| &*self.arena.alloc(d))
                .map(UnitAst::Decl),
            Dot => self.unex_tok(token, "dot can only follow an expression")?,
            DoubleDot => self.unex_tok(token, "double dot can only follow an expression")?,
            Int => self.int(token),
            True => self.bool(token, true),
            False => self.bool(token, false),
            Op(op) => self.unary(
                diver.untyped_dive(),
                OpAst {
                    span: token.span,
                    kind: op,
                },
            ),
            Eof => self.unex_tok(token, "got eof when expression is expected")?,
            Err => self.unex_tok(token, "got error token when expression is expected")?,
        }?;

        self.handle_postfix(diver, expr)
    }

    fn record(
        &mut self,
        mut diver: Diver,
        kind: RecordKind,
        keyword: Span,
    ) -> Option<UnitAst<'arena>> {
        self.expect_advance(LBrace, "expected left brace to start record definition")?;

        let mut items = diver.dive::<RecordItemAst<'arena>>();

        while self.try_advance(RBrace).is_none() {
            self.skip_semis();
            let expr = self.record_item(items.untyped_dive())?;
            items.push(expr);
        }

        Some(UnitAst::Record(RecordAst {
            kind,
            keyword,
            fields: self.arena.alloc_rev_iter(items),
        }))
    }

    fn record_item(&mut self, diver: Diver) -> Option<RecordItemAst<'arena>> {
        Some(RecordItemAst {
            name: self.ident("field name")?,
            colon: self
                .expect_advance(Colon, "epected colon to separate your mom")?
                .span,
            value: self.expr(diver)?,
        })
    }

    fn return_expr(&mut self, diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        let expr = self
            .has_next(Self::NON_EXPR_TOKENS)
            .not()
            .then(|| self.expr(diver))
            .transpose()?;
        Some(UnitAst::Return(
            self.arena.alloc(ReturnAst { keyword, expr }),
        ))
    }

    fn rt(&mut self, diver: Diver, _keyword: Span) -> Option<UnitAst<'arena>> {
        let expr = self.expr(diver)?;
        Some(UnitAst::Rt(self.arena.alloc(expr)))
    }

    fn if_expr(&mut self, mut diver: Diver, keyword: crate::Span) -> Option<UnitAst<'arena>> {
        let cond = self.expr(diver.untyped_dive())?;
        let then = self.expr(diver.untyped_dive())?;
        let else_ = self
            .try_advance(Else)
            .map(|_| self.expr(diver))
            .transpose()?;

        Some(UnitAst::If(self.arena.alloc(IfAst {
            keyword,
            cond,
            then,
            else_,
        })))
    }

    fn decl(&mut self, mut diver: Diver, keyword: Span) -> Option<DeclAst<'arena>> {
        let mutable = self.try_advance(Mut).map(|t| t.span);
        let name = self.ident("decl name")?;
        let ty = self
            .try_advance(Colon)
            .map(|_| self.expr(diver.untyped_dive()))
            .transpose()?;

        let value = self
            .try_advance(Op(OpCode::Assign))
            .map(|_| self.expr(diver.untyped_dive()))
            .transpose()?;

        Some(DeclAst {
            keyword,
            mutable,
            name,
            ty,
            value,
        })
    }

    fn handle_postfix(
        &mut self,
        mut diver: Diver,
        mut expr: UnitAst<'arena>,
    ) -> Option<UnitAst<'arena>> {
        loop {
            let token = self.peek();
            expr = match token.kind {
                LParen => {
                    self.next();
                    let args = &*self
                        .sequence(
                            diver.untyped_dive(),
                            Self::expr,
                            Comma,
                            RParen,
                            "function parameter",
                        )?
                        .0;

                    UnitAst::Call(self.arena.alloc(CallAst { caller: expr, args }))
                }
                Dot => {
                    self.next();
                    if let Some(b) = self.try_advance(LBrace) {
                        self.ctor(diver.untyped_dive(), b.span, expr)?
                    } else {
                        let name = self.ident("field name")?;
                        UnitAst::Field(self.arena.alloc(FieldAst {
                            on: expr,
                            dot: token.span,
                            name,
                        }))
                    }
                }
                _ => break Some(expr),
            }
        }
    }

    fn ctor(
        &mut self,
        mut diver: Diver,
        brace: Span,
        ty: UnitAst<'arena>,
    ) -> Option<UnitAst<'arena>> {
        let mut items = diver.dive::<CtorField<'arena>>();
        let mut fill: Option<result::Result<ExprAst, Span>> = None;

        while self.try_advance(RBrace).is_none() {
            if let Some(fill) = &fill {
                let span = self.peek().span;
                self.diags
                    .builder(self.files)
                    .footer(
                        Severty::Error,
                        "filling unset fields can only be done after field inits",
                    )
                    .annotation(
                        Severty::Note,
                        fill.as_ref().map_or_else(|p| *p, |e| e.span()),
                        "previous fill",
                    )
                    .annotation(Severty::Error, span, "excess fill");
                return None;
            }

            match self.ctor_field(items.untyped_dive())? {
                CtorItemAst::Field(f) => drop(items.push(f)),
                CtorItemAst::Default(d) => fill = Some(result::Result::Err(d)),
                CtorItemAst::Fill(expr) => fill = Some(result::Result::Ok(expr)),
            }

            if self.try_advance(RBrace).is_some() {
                break;
            }

            self.expect_advance(Comma, "comma or closing brace")?;
        }

        Some(UnitAst::Ctor(self.arena.alloc(CtorAst {
            brace,
            ty,
            fill: fill.map(result::Result::ok),
            fields: self.arena.alloc_rev_iter(items),
        })))
    }

    fn ctor_field(&mut self, diver: Diver) -> Option<CtorItemAst<'arena>> {
        let tok = self.peek();

        match tok.kind {
            Ident => {
                let name = self.ident("field name")?;
                let value = self
                    .try_advance(Colon)
                    .map(|_| self.expr(diver))
                    .transpose()?;
                Some(CtorItemAst::Field(CtorField { name, value }))
            }
            DoubleDot => {
                self.next();
                if self.has_next([Comma, RBrace]) {
                    Some(CtorItemAst::Default(tok.span))
                } else {
                    let expr = self.expr(diver)?;
                    Some(CtorItemAst::Fill(expr))
                }
            }
            _ => self.unex_tok(tok, "expected field name or ..")?,
        }
    }

    fn unary(&mut self, diver: Diver, op: OpAst) -> Option<UnitAst<'arena>> {
        match op.kind {
            _ => {
                let expr = self.unit(diver)?;
                Some(UnitAst::Unary(self.arena.alloc(UnaryAst { op, expr })))
            }
        }
    }

    fn func(&mut self, mut diver: Diver, keyword: Span) -> Option<UnitAst<'arena>> {
        self.expect_advance(LParen, "expected start of argument list")?;
        let (args, _) = self.sequence(
            diver.untyped_dive(),
            Self::func_arg,
            Comma,
            RParen,
            "function parameter",
        )?;

        let return_ty = self
            .try_advance(Colon)
            .map(|_| self.expr(diver.untyped_dive()))
            .transpose()?;

        let body = self.expr(diver)?;

        Some(UnitAst::Func(self.arena.alloc(FuncAst {
            args,
            return_ty,
            body,
            keyword,
        })))
    }

    fn func_arg(&mut self, diver: Diver) -> Option<FuncArgAst<'arena>> {
        let name = self.ident("parameter name")?;
        let colon = self
            .expect_advance(Colon, "expected colon after parameter name")?
            .span;
        let ty = self.expr(diver)?;

        Some(FuncArgAst { name, colon, ty })
    }

    fn int(&mut self, token: Token<'src>) -> Option<UnitAst<'arena>> {
        let value = token
            .source
            .parse::<u64>()
            .expect("lexer should have validated int");
        Some(UnitAst::Literal(LitAst {
            span: token.span,
            kind: LitKindAst::Int(IntLit::new(value)),
        }))
    }

    fn paren(&mut self, mut diver: Diver) -> Option<UnitAst<'arena>> {
        let expr = self.expr(diver.untyped_dive())?;
        self.expect_advance(RParen, "right parenthesis")?;
        Some(UnitAst::Paren(self.arena.alloc(expr)))
    }

    fn block(&mut self, diver: Diver, brace: Span) -> Option<UnitAst<'arena>> {
        self.block_low(diver, brace).map(UnitAst::Block)
    }

    fn block_low(&mut self, mut diver: Diver, brace: Span) -> Option<BlockAst<'arena>> {
        let mut exprs = diver.dive::<ExprAst<'arena>>();

        let mut trailing_semi;
        loop {
            trailing_semi = self.skip_semis() || exprs.is_empty();
            if self.try_advance(RBrace).is_some() {
                break;
            }

            let expr = self.expr(exprs.untyped_dive())?;
            exprs.push(expr);
        }

        Some(BlockAst {
            brace,
            exprs: self.arena.alloc_rev_iter(exprs),
            trailing_semi,
        })
    }

    fn bool(&self, token: Token, value: bool) -> Option<crate::UnitAst<'arena>> {
        Some(crate::UnitAst::Literal(crate::LitAst {
            span: token.span,
            kind: crate::LitKindAst::Bool(value),
        }))
    }

    fn ident(&mut self, objective: impl fmt::Display) -> Option<IdentAst> {
        let token =
            self.expect_advance(Ident, format_args!("expected identifier for {}", objective))?;

        Some(IdentAst {
            span: token.span,
            ident: IdentStr::from_str(token.source),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ModItemAst<'arena> {
    Decl(DeclAst<'arena>),
}

#[derive(Debug, Clone)]
pub enum ExprAst<'arena> {
    Unit(UnitAst<'arena>),
    Binary(&'arena BinaryAst<'arena>),
}

impl ExprAst<'_> {
    pub fn span(&self) -> Span {
        match self {
            ExprAst::Unit(unit) => unit.span(),
            ExprAst::Binary(binary) => binary.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnitAst<'arena> {
    Literal(LitAst),
    Ident(IdentAst),
    Import(IdentAst),

    If(&'arena IfAst<'arena>),
    Block(BlockAst<'arena>),
    Unary(&'arena UnaryAst<'arena>),
    Call(&'arena CallAst<'arena>),
    Func(&'arena FuncAst<'arena>),
    Record(RecordAst<'arena>),
    Decl(&'arena DeclAst<'arena>),
    Paren(&'arena ExprAst<'arena>),
    Field(&'arena FieldAst<'arena>),
    Rt(&'arena ExprAst<'arena>),
    Return(&'arena ReturnAst<'arena>),
    Ctor(&'arena CtorAst<'arena>),
}

impl UnitAst<'_> {
    pub fn span(&self) -> Span {
        use UnitAst::*;
        match self {
            Literal(literal) => literal.span,
            Ident(ident) | Import(ident) => ident.span,
            If(if_) => if_.keyword,
            Block(block) => block.brace,
            Unary(unary) => unary.op.span,
            Call(call) => call.caller.span(),
            Func(func) => func.keyword,
            Record(rec) => rec.keyword,
            Decl(decl) => decl.keyword,
            Paren(expr) | Rt(expr) => expr.span(),
            Field(field) => field.dot,
            Return(r) => r.keyword,
            Ctor(ctor) => ctor.brace,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CtorAst<'arena> {
    pub brace: Span,
    pub ty: UnitAst<'arena>,
    pub fill: Option<Option<ExprAst<'arena>>>,
    pub fields: &'arena [CtorField<'arena>],
}

pub enum CtorItemAst<'arena> {
    Field(CtorField<'arena>),
    Default(Span),
    Fill(ExprAst<'arena>),
}

#[derive(Debug, Clone)]
pub struct CtorField<'arena> {
    pub name: IdentAst,
    pub value: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone)]
pub struct RecordAst<'arena> {
    pub kind: RecordKind,
    pub keyword: Span,
    pub fields: &'arena [RecordItemAst<'arena>],
}

#[derive(Debug, Clone)]
pub struct RecordItemAst<'arena> {
    pub name: IdentAst,
    pub colon: Span,
    pub value: ExprAst<'arena>,
}

#[derive(Debug, Clone)]
pub struct ReturnAst<'arena> {
    pub keyword: Span,
    pub expr: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone)]
pub struct IfAst<'arena> {
    pub keyword: Span,
    pub cond: ExprAst<'arena>,
    pub then: ExprAst<'arena>,
    pub else_: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone)]
pub struct FieldAst<'arena> {
    pub on: UnitAst<'arena>,
    pub dot: Span,
    pub name: IdentAst,
}

#[derive(Debug, Clone)]
pub struct DeclAst<'arena> {
    pub keyword: Span,
    pub mutable: Option<Span>,
    pub name: IdentAst,
    pub ty: Option<ExprAst<'arena>>,
    pub value: Option<ExprAst<'arena>>,
}

#[derive(Debug, Clone)]
pub struct BlockAst<'arena> {
    pub brace: Span,
    pub exprs: &'arena [ExprAst<'arena>],
    pub trailing_semi: bool,
}

#[derive(Debug, Clone)]
pub struct UnaryAst<'arena> {
    pub op: OpAst,
    pub expr: UnitAst<'arena>,
}

#[derive(Debug, Clone)]
pub struct FuncAst<'arena> {
    pub keyword: Span,
    pub args: &'arena [FuncArgAst<'arena>],
    pub return_ty: Option<ExprAst<'arena>>,
    pub body: ExprAst<'arena>,
}

#[derive(Debug, Clone)]
pub struct FuncArgAst<'arena> {
    pub name: IdentAst,
    pub colon: Span,
    pub ty: ExprAst<'arena>,
}

#[derive(Debug, Clone)]
pub struct CallAst<'arena> {
    pub caller: UnitAst<'arena>,
    pub args: &'arena [ExprAst<'arena>],
}

#[derive(Debug, Clone)]
pub struct BinaryAst<'arena> {
    pub lhs: ExprAst<'arena>,
    pub rhs: ExprAst<'arena>,
    pub op: OpAst,
}

impl BinaryAst<'_> {
    fn span(&self) -> Span {
        self.lhs.span()
    }
}

#[derive(Debug, Clone)]
pub struct OpAst {
    pub kind: OpCode,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LitAst {
    pub kind: LitKindAst,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntLit([u8; 8]);

impl IntLit {
    pub fn new(value: u64) -> Self {
        Self(value.to_ne_bytes())
    }

    pub fn value(&self) -> u64 {
        u64::from_ne_bytes(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LitKindAst {
    Int(IntLit),
    Bool(bool),
}

impl fmt::Display for LitKindAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitKindAst::Int(int) => int.value().fmt(f),
            LitKindAst::Bool(bool) => bool.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentAst {
    pub ident: IdentStr,
    pub span: Span,
}

#[cfg(test)]
mod test {
    use crate::{format_ast, Diagnostics, File, Files, Parser};
    use mini_alloc::{ArenaBase, DiverBase};

    fn perform_test(_: &str, source_code: &str, ctx: &mut String) {
        let mut files = Files::new();
        let file = File::new("test".into(), source_code.into());
        let file_id = files.push(file);
        let mut diags = Diagnostics::default();
        let mut arena = ArenaBase::new(1000);
        let scope = arena.scope();
        let mut diver = DiverBase::new(1000);

        let res = Parser::new(&files, file_id, &mut diags, &scope).parse(diver.dive());

        match res {
            Some(ast) => format_ast(ast, ctx, 0),
            None => ctx.push_str(diags.view()),
        }
    }

    crate::print_cases! { perform_test:
        precedence "let val = 1 + 2 * 3 - 4 / 5";
        function "let add2 = fn(a: int): int { a + 2 }";
        function_call "let res = foo(1, 2, 3)";
    }
}
