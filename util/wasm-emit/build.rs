/*
use std::{fs, path::PathBuf, process::Command};

use document_tree::{
    element_categories::{BodyElement, StructuralSubElement, SubStructure, TextOrInlineElement},
    *,
};
use rst_parser;
*/

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    /*

        download_wasm_spec();

        let [mut instructions] = load_and_parse_documents();

        fn extract_children(elem: &mut StructuralSubElement) -> &mut [StructuralSubElement] {
            match elem {
                StructuralSubElement::SubStructure(s) => match &mut **s {
                    SubStructure::Section(s) => s.children_mut(),
                    _ => &mut [],
                },
                _ => &mut [],
            }
        }

        fn capitalize(s: &str) -> impl Iterator<Item = char> + '_ {
            let mut chars = s.chars();
            let first = chars.next().map(|c| c.to_uppercase());
            first
                .into_iter()
                .flatten()
                .chain(chars.map(char::to_lowercase).flatten())
        }

        fn is_ident(s: &str) -> bool {
            s.chars().all(|c| c.is_ascii_alphanumeric())
        }

        let mut missed_instrs = vec![];
        let mut missed_lines = vec![];

        let mut instrs = instructions
            .children_mut()
            .iter_mut()
            .flat_map(extract_children)
            .flat_map(extract_children)
            .filter_map(|s| match s {
                StructuralSubElement::SubStructure(s) => Some(s),
                _ => None,
            })
            .filter_map(|s| match &mut **s {
                SubStructure::BodyElement(b) => Some(b),
                _ => None,
            })
            .filter_map(|s| match &mut **s {
                BodyElement::Comment(p) => Some(p),
                _ => None,
            })
            .flat_map(|s| s.children_mut())
            .filter_map(|s| match s {
                TextOrInlineElement::String(s) => Some(s),
                _ => None,
            })
            .filter_map(|s| s.strip_prefix("  \\hex{"))
            .filter_map(|s| s.split_once('}'))
            .filter_map(|(byte, rest)| {
                Some((
                    byte,
                    rest.split_once(" &\\Rightarrow& ").or_else(|| {
                        missed_lines.push((byte, rest));
                        None
                    })?,
                ))
            })
            .map(|(byte, (args, name))| {
                (
                    byte,
                    args,
                    name.split_once(|c| c == ' ' || c == '~')
                        .map_or(name, |s| s.0),
                )
            })
            .map(|(byte, args, name)| {
                let args = args
                    .split("~")
                    .filter(|&s| !s.is_empty())
                    .map(|s| {
                        s.split_once("{:}\\B")
                            .or_else(|| {
                                s.strip_prefix("\\hex{")
                                    .and_then(|s| s.strip_suffix("}"))
                                    .map(|s| (s, "u32"))
                            })
                            .expect(s)
                    })
                    .collect::<Vec<_>>();
                let name = name
                    .split(|c| ".\\{}_".contains(c))
                    .filter(|&s| s != "K")
                    .flat_map(capitalize)
                    .collect::<String>();
                (byte, args, name)
            })
            .filter_map(|(byte, args, name)| {
                let pred = args
                    .iter()
                    .all(|&(name, ty)| is_ident(name) && is_ident(ty));
                if !pred {
                    missed_instrs.push((byte, args, name));
                    None
                } else {
                    Some((byte, args, name))
                }
            })
            .collect::<Vec<_>>();

        let one_bytes = instrs
            .drain_filter(|(_, args, _)| args.len() == 0)
            .collect::<Vec<_>>();
        let multy_bytes = instrs
            .drain_filter(|(_, args, _)| matches!(args.as_slice(), [(s, "u32")] if u32::from_str_radix(s, 16).is_ok())).collect::<Vec<_>>();

        use std::fmt::Write;
        let mut generated = String::new();

        generated.push_str("use crate::*;\n");
        generated.push_str("gen_instrs! {\n");

        generated.push_str("\tone_bytes: [\n");
        // skip Epsilon, which sneaks in the first position
        for (byte, _, name) in one_bytes.into_iter().skip(1) {
            writeln!(generated, "\t\t0x{} => {}", byte, name).unwrap();
        }
        generated.push_str("\t],\n");

        writeln!(generated, "\tmulty_bytes: [").unwrap();
        for (byte, args, name) in multy_bytes {
            writeln!(generated, "\t\t0x{} {} => {}", byte, args[0].0, name).unwrap();
        }
        generated.push_str("\t],\n");

        fn to_snake_case(s: &str) -> String {
            let mut result = String::new();
            for c in s.chars() {
                if c.is_ascii_uppercase() {
                    if !result.is_empty() {
                        result.push('_');
                    }
                    result.push(c.to_ascii_lowercase());
                } else {
                    result.push(c);
                }
            }
            result
        }

        fn is_builtin_type(ty: &str) -> bool {
            matches!(
                ty,
                "u32" | "i32" | "u64" | "i64" | "u128" | "i128" | "f32" | "f64"
            )
        }

        generated.push_str("\tothers: [\n");
        for (byte, args, name) in instrs {
            let name = to_snake_case(&name);
            write!(generated, "\t\t0x{} => {}(", byte, name).unwrap();
            let mut written = false;
            for (name, ty) in &args {
                if u32::from_str_radix(name, 16).is_ok() {
                    continue;
                }

                if written {
                    generated.push_str(", ");
                }

                written = true;

                if is_builtin_type(ty) {
                    write!(generated, "{}: {}", name, ty).unwrap();
                } else {
                    let ty = capitalize(ty).collect::<String>();
                    write!(generated, "{}: {}", name, ty).unwrap();
                }
            }
            generated.push_str(") { ");

            for (name, _) in args {
                write!(generated, "{name} ").unwrap();
            }
            generated.push_str("}\n");
        }
        generated.push_str("\t],\n");

        generated.push_str("}\n");

        for (byte, args, name) in missed_instrs {
            writeln!(generated, "// 0x{} {} {:?}", byte, name, args).unwrap();
        }

        for (byte, rest) in missed_lines {
            writeln!(generated, "// 0x{} {}", byte, rest.trim()).unwrap();
        }

        fs::create_dir_all("src/expr").unwrap();
        fs::write("src/expr/generated_instrs.rs", generated).unwrap();

    }

    fn load_and_parse_documents() -> [document_tree::Document; 1] {
        ["instructions"]
            .map(|name| PathBuf::from_iter(["spec", "document", "core", "binary", name]))
            .map(|path| path.with_extension("rst"))
            .map(|path| std::fs::read_to_string(&path).expect(&path.to_string_lossy()))
            .map(|text| rst_parser::parse(&text).unwrap())
    }

    fn download_wasm_spec() {
        Command::new("git")
            .args([
                "clone",
                "--depth",
                "1",
                "https://github.com/WebAssembly/spec.git",
            ])
            .status()
            .unwrap();
        */
}
