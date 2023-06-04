use crate::*;
use mini_alloc::*;

pub fn format_instrs(instrs: &[Instr], ctx: &mut String, interner: &StrInterner) {
    for &instr in instrs {
        format_instr(instr, ctx, interner);
    }
}

fn format_instr(instr: Instr, ctx: &mut String, _: &StrInterner) {
    use std::fmt::Write;
    macro write($($arg:tt)*) {
        std::write!(ctx, $($arg)*).unwrap()
    }
    match instr {
        Instr::Const(c) => write!("const{}", c.index()),
        Instr::Ident(i) => write!("ident{}", i.index()),
        Instr::Import(i) => write!("import{}", i.index()),
        Instr::Block { expr_count } => write!("block{}", expr_count),
        Instr::ExprBlock { expr_count } => write!("expr_block{}", expr_count),
        Instr::Array { item_count } => write!("array{}", item_count),
        Instr::FilledArray => write!("filled_array"),
        Instr::Tuple { item_count } => write!("tuple{}", item_count),
        Instr::Struct { field_count } => write!("struct{}", field_count),
        Instr::StructField(name) => write!("struct_field ident{}", name.index()),
        Instr::InlinedField(name) => write!("inlined_field ident{}", name.index()),
        Instr::Embed => write!("embed"),
        Instr::Enum(name) => write!("enum ident{}", name.index()),
        Instr::UnitEnum(name) => write!("unit_enum ident{}", name.index()),
        Instr::Call { arg_count } => write!("call {}", arg_count),
        Instr::Func(func) => write!("func{}", func.index()),
        Instr::Unary(op) => write!("unary {}", op.name()),
        Instr::Binary(op) => write!("binary {}", op.name()),
        Instr::Index => write!("index"),
        Instr::Decl => write!("decl"),
        Instr::Ret { has_value } => write!("ret {}", has_value),
        Instr::Loop => write!("loop"),
        Instr::Continue(l) => write!("continue loop{}", l.index()),
        Instr::Break(l) => write!("break loop{}", l.index()),
        Instr::ValuelessBreak(l) => write!("valueless_break loop{}", l.index()),
        Instr::If { instr_count } => write!("if {}", instr_count),
        Instr::Else { instr_count } => write!("else {}", instr_count),
        Instr::Field(name) => write!("field ident{}", name.index()),
        Instr::Unkown => write!("unkown"),
    }
}
