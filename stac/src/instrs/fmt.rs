use crate::{BitSet, FuncMetaView, InstrKind};
use mini_alloc::StrInterner;
use std::fmt::Write;

pub fn format_instrs(
    func_meta: FuncMetaView,
    ctx: &mut String,
    prefix: &str,
    interner: &StrInterner,
) {
    let mut jump_targets = BitSet::with_capacity(func_meta.instrs.len());
    for &instr in func_meta.instrs {
        if let InstrKind::Jump { to, .. } = instr.kind {
            jump_targets.insert(to as usize);
        }
    }
    for (i, &instr) in func_meta.instrs.iter().enumerate() {
        ctx.push_str(prefix);
        if jump_targets.contains(i) {
            write!(ctx, "{:03} ", i).unwrap();
        } else {
            write!(ctx, "    ").unwrap();
        }
        format_instr(instr.kind, func_meta, ctx);
        ctx.push('\n');
    }

    if jump_targets.contains(func_meta.instrs.len()) {
        ctx.push_str(prefix);
        write!(ctx, "{:03}\n", func_meta.instrs.len()).unwrap();
    }
}

fn format_instr(
    instr: InstrKind,
    func_meta: FuncMetaView,
    ctx: &mut String,
    interner: &StrInterner,
) {
    macro write($($arg:tt)*) {
        std::write!(ctx, $($arg)*).unwrap()
    }

    match instr {
        InstrKind::Const(c) => write!("const {}", func_meta.consts[c].display(interner)),
        InstrKind::Sym(i) => write!("sym{}", i.index()),
        InstrKind::Mod(i) => write!("mod{}", i.index()),
        InstrKind::Array { item_count } => write!("array{item_count}",),
        InstrKind::FilledArray { len_const } => {
            write!(
                "filled_array len_const {}",
                func_meta.consts[len_const].display(interner)
            )
        }
        InstrKind::Tuple { item_count } => write!("tuple{item_count}",),
        InstrKind::Struct { field_count } => write!("struct{field_count}",),
        InstrKind::StructField { name, has_value } => {
            write!("struct_field ident{} {has_value}", name.index())
        }
        InstrKind::Embed => write!("embed"),
        InstrKind::Enum { name, has_value } => write!("enum ident{} {has_value}", name.index()),
        InstrKind::Call { arg_count } => write!("call {arg_count}"),
        InstrKind::Func(func) => write!("func{}", func.index()),
        InstrKind::Unary(op) => write!("unary {}", op.name()),
        InstrKind::Binary(op) => write!("binary {}", op.name()),
        InstrKind::Index => write!("index"),
        InstrKind::Decl => write!("decl"),
        InstrKind::Field { name, is_meta } => write!("field ident{} {is_meta}", name.index()),
        InstrKind::Unkown => write!("unkown"),
        InstrKind::Self_ => write!("self"),
        InstrKind::Jump { to, conditional } => write!("jump {} {}", to, conditional),
        InstrKind::BackJumpDest { used } => write!("back_jump_dest {}", used),
        InstrKind::Error => write!("error"),
        InstrKind::Drop => write!("drop"),
        InstrKind::DropDecl { decl_count } => write!("drop_decl {decl_count}"),
    }
}
