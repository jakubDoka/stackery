use mini_alloc::{ArenaScope, FnvHashMap, InternedStr, StrInterner};

use crate::{parser::expr::NamedExprAst, *};

use super::ast;

#[derive(Default)]
struct CacheParserRes {
    string_parser: StringParser,
    used_strs: FnvHashMap<InternedStr, CacheRef<InternedStr>>,
    scope: Scope,
}

impl<'a> CacheParser<'a> {
    pub fn update(mut self, temp_mem: &mut TempMemBase, meta: &ModuleMeta) {
        let mut res = CacheParserRes::default();
        self.reparse_changed(&mut res, temp_mem, meta);
    }

    fn reparse_changed(
        &mut self,
        res: &mut CacheParserRes,
        temp_mem: &mut TempMemBase,
        meta: &ModuleMeta,
    ) {
        self.clean_cache(temp_mem, meta);

        for module_ref in meta.changed() {
            let module = &mut self.modules.eintities[module_ref];

            let (diver, arena) = temp_mem.diver_and_arena();
            let Some(ast) = Parser::new(
                &self.modules.files,
                module.file,
                self.interner,
                self.diagnostics,
                &arena,
                &mut res.string_parser,
            )
            .parse(diver) else {
                continue;
            };

            let Some(parsed) = CacheAstBuilder {
                push: self.modules.cahce.push(),
                files: &self.modules.files,
                interner: self.interner,
                diags: self.diagnostics,
                res,
                deps: &self.modules.deps[module.deps],
                modules: &mut self.modules.eintities,
            }
            .build(&arena, ast, &mut self.modules.scope) else {
                continue;
            };

            self.modules.eintities[module_ref].parsed = parsed;
        }
    }

    fn clean_cache(&mut self, temp_mem: &mut TempMemBase, meta: &ModuleMeta) {
        let chunks = self
            .modules
            .eintities
            .iter_mut()
            .filter_map(|(id, m)| (!meta.is_updated(id)).then_some(&mut m.parsed.cache));
        self.modules.cahce.preserve_chunks(temp_mem, chunks);
    }
}

struct CacheAstBuilder<'a> {
    push: CachePush<'a>,
    files: &'a Files,
    interner: &'a StrInterner,
    diags: &'a mut Diagnostics,
    res: &'a mut CacheParserRes,
    deps: &'a CacheVecView<ModuleRef>,
    modules: &'a mut CachePool<Module>,
}

impl<'a> CacheAstBuilder<'a> {
    fn build(
        mut self,
        arena: &ArenaScope,
        ast: &[ExprAst],
        scope_cache: &mut ScopeCache,
    ) -> Option<ParsedModule> {
        self.res.used_strs.clear();

        let exprs = arena.alloc_iter(
            ast.iter()
                .copied()
                .map(|e| self.expr(e, &arena.proxi().scope())),
        );

        Some(ParsedModule {
            scope: self.res.scope.cache(scope_cache),
            body: self.push.exprs.extend(exprs.iter().flatten().copied()),
            cache: self.push.finish(),
        })
    }

    fn expr(&mut self, expr: ExprAst, arena: &ArenaScope) -> Option<ast::Expr> {
        match expr {
            ExprAst::Unit(b) => self.unit(b, arena),
            ExprAst::Binary(&b) => self.binary(b, arena),
        }
    }

    fn unit(&mut self, unit: UnitAst, arena: &ArenaScope) -> Option<ast::Expr> {
        match unit {
            UnitAst::Literal(l) => Some(ast::Expr::Lit(self.push.consts.push(l.kind))),
            UnitAst::Ident(i) => self.sym(i).map(ast::Expr::Ident),
            UnitAst::Import(i) => self.import(i).map(ast::Expr::Import),
            UnitAst::Str(s) => Some(ast::Expr::Str(self.string(s))),
            UnitAst::Block(&b) => self.block(b, arena),
            UnitAst::Unary(&u) => self.unary(u, arena),
            UnitAst::Array(a) => Some(ast::Expr::Array(self.expr_seq(a, arena))),
            UnitAst::FilledArray(_) => todo!(),
            UnitAst::Tuple(t) => Some(ast::Expr::Tuple(self.expr_seq(t, arena))),
            UnitAst::Struct(s) => Some(ast::Expr::Struct(self.struct_(s, arena))),
            UnitAst::Enum(&e) => self.enum_(e, arena).map(ast::Expr::Enum),
            UnitAst::Call(&c) => self.call(c, arena).map(ast::Expr::Call),
            UnitAst::Func(&f) => self.func(f, arena).map(ast::Expr::Func),
            UnitAst::Decl(&d) => self.decl(d, arena),
            UnitAst::Loop(&l) => self.loop_(l, arena),
            UnitAst::Index(&i) => self.index(i, arena),
            UnitAst::ForLoop(_) => todo!(),
            UnitAst::Break(&b) => self.break_(b, arena),
            UnitAst::Continue(c) => Some(ast::Expr::Continue(
                c.label.map(|l| self.sym(l)).transpose()?,
            )),
            UnitAst::Field(&f) => self.field_access(f, arena).map(ast::Expr::Field),
            UnitAst::If(&i) => self.if_(i, arena).map(ast::Expr::If),
            UnitAst::Ret(expr) => Some(ast::Expr::Ret(
                expr.map(|&e| self.expr(e, arena))
                    .transpose()?
                    .map(|e| self.push.exprs.push(e)),
            )),
            UnitAst::Paren(&expr) => self.expr(expr, arena),
            UnitAst::Unknown(..) => Some(ast::Expr::Unknown),
        }
    }

    fn if_(
        &mut self,
        IfAst { cond, then, else_ }: IfAst,
        arena: &ArenaScope,
    ) -> Option<CacheRef<ast::If>> {
        let cond = self.expr(cond, arena)?;
        let then = self.expr(then, arena)?;
        let else_ = else_.map(|e| self.expr(e, arena)).transpose()?;
        Some(self.push.ifs.push(ast::If { cond, then, else_ }))
    }

    fn field_access(
        &mut self,
        FieldAst { expr, field }: FieldAst,
        arena: &ArenaScope,
    ) -> Option<CacheRef<ast::FieldAccess>> {
        let expr = self.unit(expr, arena)?;
        let res = ast::FieldAccess {
            expr,
            field: field.name,
        };
        Some(self.push.fields.push(res))
    }

    fn break_(
        &mut self,
        BreakAst { label, expr }: BreakAst,
        arena: &ArenaScope,
    ) -> Option<ast::Expr> {
        let label = label.map(|l| self.sym(l)).transpose()?;
        let expr = expr.map(|e| self.expr(e, arena)).transpose()?;
        Some(ast::Expr::Break(
            label,
            expr.map(|e| self.push.exprs.push(e)),
        ))
    }

    fn index(
        &mut self,
        IndexAst { expr, index }: IndexAst,
        arena: &ArenaScope,
    ) -> Option<ast::Expr> {
        let array = self.unit(expr, arena)?;
        let index = self.expr(index, arena)?;
        Some(ast::Expr::Index(
            self.push.exprs.push(array),
            self.push.exprs.push(index),
        ))
    }

    fn loop_(&mut self, LoopAst { body, label }: LoopAst, arena: &ArenaScope) -> Option<ast::Expr> {
        let frame = self.res.scope.start_frame();
        let label = label.map(|l| self.res.scope.push(l.name));
        let body = self.expr(body, arena)?;
        self.res.scope.end_frame(frame);

        Some(ast::Expr::Loop(label, self.push.exprs.push(body)))
    }

    fn decl(
        &mut self,
        NamedExprAst { name, expr }: NamedExprAst,
        arena: &ArenaScope,
    ) -> Option<ast::Expr> {
        let name = self.res.scope.push(name.name);
        let value = self.expr(expr, arena)?;
        Some(ast::Expr::Decl(name, self.push.exprs.push(value)))
    }

    fn func(
        &mut self,
        FuncAst { args, body }: FuncAst,
        arena: &ArenaScope,
    ) -> Option<CacheRef<ast::Func>> {
        let frame = self.res.scope.start_frame();
        let res = ast::Func {
            args: self.func_args(args, arena),
            body: self.expr(body, arena)?,
        };
        self.res.scope.end_frame(frame);
        Some(self.push.funcs.push(res))
    }

    fn func_args(&mut self, args: &[FuncArgAst], arena: &ArenaScope) -> CacheSlice<ast::FuncArg> {
        let args = arena.alloc_iter(args.iter().map(|a| {
            let name = self.res.scope.push(a.name.name);
            let default = a.default.map(|d| self.unit(d, arena)).transpose()?;
            Some(ast::FuncArg { name, default })
        }));
        self.push.func_args.extend(args.iter().flatten().copied())
    }

    fn call(
        &mut self,
        CallAst { caller, args }: CallAst,
        arena: &ArenaScope,
    ) -> Option<CacheRef<ast::Call>> {
        let res = ast::Call {
            caller: self.unit(caller, arena)?,
            args: self.expr_seq(args, arena),
        };
        Some(self.push.calls.push(res))
    }

    fn enum_(
        &mut self,
        EnumAst { name, value }: EnumAst,
        arena: &ArenaScope,
    ) -> Option<CacheRef<ast::Enum>> {
        let res = ast::Enum {
            name: name.name,
            value: value.map(|v| self.expr(v, arena)).transpose()?,
        };
        Some(self.push.enums.push(res))
    }

    fn struct_(
        &mut self,
        struct_: &[StructFieldAst],
        arena: &ArenaScope,
    ) -> CacheSlice<ast::StructField> {
        let fields = arena.alloc_iter(struct_.iter().map(|&f| {
            Some(match f {
                StructFieldAst::Decl(n) => ast::StructField::Named {
                    name: n.name.name,
                    value: self.expr(n.expr, arena)?,
                },
                StructFieldAst::Inline(i) => ast::StructField::Inline(i.name),
                StructFieldAst::Embed(e) => ast::StructField::Embed(self.expr(e, arena)?),
            })
        }));

        self.push
            .struct_fields
            .extend(fields.iter().flatten().copied())
    }

    fn expr_seq(&mut self, array: &[ExprAst], arena: &ArenaScope) -> CacheSlice<ast::Expr> {
        let exprs = arena.alloc_iter(array.iter().copied().map(|e| self.expr(e, arena)));
        self.push.exprs.extend(exprs.iter().flatten().copied())
    }

    fn unary(&mut self, unary: UnaryAst, arena: &ArenaScope) -> Option<ast::Expr> {
        let base = self.unit(unary.expr, arena)?;
        let op = unary.op.kind;

        Some(ast::Expr::Unary(op, self.push.exprs.push(base)))
    }

    fn block(
        &mut self,
        BlockAst {
            exprs,
            trailing_semi,
        }: BlockAst,
        arena: &ArenaScope,
    ) -> Option<ast::Expr> {
        let frame = self.res.scope.start_frame();
        let exprs = self.expr_seq(exprs, arena);
        self.res.scope.end_frame(frame);

        Some(ast::Expr::Block {
            trailing_semi,
            exprs,
        })
    }

    fn string(&mut self, string: IdentAst) -> CacheRef<InternedStr> {
        *self
            .res
            .used_strs
            .entry(string.name)
            .or_insert_with(|| self.push.strs.push(string.name))
    }

    fn import(&mut self, import: IdentAst) -> Option<ModuleRef> {
        self.deps
            .iter()
            .map(|&m| (m, self.files[self.modules[m].file].name()))
            .find_map(|(m, name)| (name == import.name).then_some(m))
    }

    fn sym(&mut self, ident: IdentAst) -> Option<Sym> {
        if let Some(sym) = self.res.scope.resolve(ident.name) {
            return Some(sym);
        }

        self.diags
            .builder(self.files, self.interner)
            .footer(
                Severty::Error,
                format_args!("symbol named '{}' not found", &self.interner[ident.name]),
            )
            .annotation(Severty::Error, ident.span, "this ident does not exist")
            .terminate()?;
    }

    fn binary(
        &mut self,
        BinaryAst { lhs, rhs, op }: BinaryAst,
        arena: &ArenaScope,
    ) -> Option<ast::Expr> {
        let lhs = self.expr(lhs, arena)?;
        let rhs = self.expr(rhs, arena)?;

        Some(ast::Expr::Binary(
            self.push.exprs.push(lhs),
            op.kind,
            self.push.exprs.push(rhs),
        ))
    }
}

#[cfg(test)]
mod test {
    use pollster::FutureExt;

    use crate::cache::test_util::LoaderMock;

    fn perform_test(sources: &str, ctx: &mut String) {
        let mut loader = LoaderMock::new(sources);

        let mut diagnostics = crate::Diagnostics::default();
        let mut modules = crate::Modules::new();
        let interner = mini_alloc::StrInterner::default();

        let root = interner.intern("root");

        let loader_ctx =
            crate::CacheLoader::new(&mut loader, &mut modules, &interner, &mut diagnostics);

        let meta = loader_ctx.update(root).block_on();

        if !diagnostics.diagnostic_view().is_empty() {
            ctx.push_str("loader diagnostics:\n");
            ctx.push_str(diagnostics.diagnostic_view());
        }

        let Some(meta) = meta else {
            ctx.push_str("no meta");
            return;
        };

        let mut temp_mem = crate::TempMemBase::new();
        let cahce_ctx = crate::CacheParser::new(&mut modules, &interner, &mut diagnostics);

        cahce_ctx.update(&mut temp_mem, &meta);
    }

    macro_rules! cases {
        ($(
            $name:ident $sources:literal
        )*) => {print_test::cases! {$(
            fn $name(ctx) {
                perform_test($sources, ctx);
            }
        )*}};
    }
}
