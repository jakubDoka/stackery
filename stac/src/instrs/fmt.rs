use crate::*;
use mini_alloc::*;
use std::fmt::Write;

pub fn format_instrs(instrs: &[Instr], ctx: &mut String, prefix: &str, interner: &StrInterner) {
    let mut jump_targets = BitSet::with_capacity(instrs.len());
    for &instr in instrs {
        if let Instr::Jump { to, .. } = instr {
            jump_targets.insert(to as usize);
        }
    }
    for (i, &instr) in instrs.iter().enumerate() {
        ctx.push_str(prefix);
        if jump_targets.contains(i) {
            write!(ctx, "{:03} ", i).unwrap();
        } else {
            write!(ctx, "    ").unwrap();
        }
        format_instr(instr, ctx, interner);
        ctx.push('\n');
    }

    if jump_targets.contains(instrs.len()) {
        ctx.push_str(prefix);
        write!(ctx, "{:03}\n", instrs.len()).unwrap();
    }
}

fn format_instr(instr: Instr, ctx: &mut String, _: &StrInterner) {
    macro write($($arg:tt)*) {
        std::write!(ctx, $($arg)*).unwrap()
    }

    match instr {
        Instr::Const(c) => write!("const{}", c.index()),
        Instr::Sym(i) => write!("sym{}", i.index()),
        Instr::Mod(i) => write!("mod{}", i.index()),
        Instr::Array { item_count } => write!("array{item_count}",),
        Instr::FilledArray => write!("filled_array"),
        Instr::Tuple { item_count } => write!("tuple{item_count}",),
        Instr::Struct { field_count } => write!("struct{field_count}",),
        Instr::StructField { name, has_value } => {
            write!("struct_field ident{} {has_value}", name.index())
        }
        Instr::Embed => write!("embed"),
        Instr::Enum { name, has_value } => write!("enum ident{} {has_value}", name.index()),
        Instr::Call { arg_count } => write!("call {arg_count}"),
        Instr::Func(func) => write!("func{}", func.index()),
        Instr::Unary(op) => write!("unary {}", op.name()),
        Instr::Binary(op) => write!("binary {}", op.name()),
        Instr::Index => write!("index"),
        Instr::Decl => write!("decl"),
        Instr::Field { name, is_meta } => write!("field ident{} {is_meta}", name.index()),
        Instr::Unkown => write!("unkown"),
        Instr::Jump { to, conditional } => write!("jump {} {}", to, conditional),
        Instr::BackJumpDest { used } => write!("back_jump_dest {}", used),
        Instr::Error => write!("error"),
        Instr::Drop => write!("drop"),
        Instr::DropDecl { decl_count } => write!("drop_decl {decl_count}"),
    }
}
