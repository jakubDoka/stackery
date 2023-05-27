use crate::sections::Limits;
use crate::*;
use alloc::vec;
use alloc::vec::Vec;

fn perform_test<'a>(tester: impl FnOnce(Encoder<DefaultBackend>) -> Module<'a>) -> Vec<u8> {
    let mut buffer = vec![];
    let mut integers = vec![];
    let mut emited: Vec<u8> = vec![];

    let module = tester(Encoder::<DefaultBackend>::new(&mut integers, &mut buffer));
    module
        .emit(
            Encoder::<DefaultBackend>::new(&mut integers, &mut buffer),
            &mut emited,
        )
        .unwrap();

    wasmparser::validate(&emited).unwrap_or_else(|_| panic!("{emited:?}"));

    emited
}

#[test]
fn default_module() {
    perform_test(|_| Module::default());
}

#[test]
fn custom_section() {
    perform_test(|_| Module {
        custom_sections: &[CustomSection {
            name: "test",
            bytes: &[1, 2, 3],
        }],
        ..Module::default()
    });

    perform_test(|_| Module {
        custom_sections: &[
            CustomSection {
                name: "",
                bytes: &[],
            },
            CustomSection {
                name: "",
                bytes: &[],
            },
        ],
        ..Module::default()
    });
}

#[test]
fn types_section() {
    perform_test(|mut e| Module {
        types: {
            let mut f = e.types();
            {
                let (_, mut e) = f.encoder();
                e.push(Valtype::Num(Numtype::I32));

                let mut e = e.returns();
                e.push(Valtype::Num(Numtype::I32));
                e.finish();
            }

            f.finish()
        },
        ..Module::default()
    });
}

#[test]
fn test_elements() {
    perform_test(|mut e| {
        let func;
        let table;
        let signature;
        Module {
            types: {
                let mut f = e.types();
                {
                    let (id, mut e) = f.encoder();
                    signature = id;
                    e.push(Valtype::Num(Numtype::I32));

                    let mut e = e.returns();
                    e.push(Valtype::Num(Numtype::I32));
                    e.finish();
                }
                f.finish()
            },
            imports: {
                let mut f = e.imports();
                table = f.import(
                    "g",
                    "m",
                    Tabletype {
                        elem_type: Reftype::FuncRef,
                        limits: Limits::Bounded { min: 0, max: 3 },
                    },
                );
                func = f.import("f", "m", signature);
                f.finish().0
            },
            elements: {
                let mut f = e.elements();
                {
                    let (.., e) = f.encoder();
                    let mut e = e.active(table);
                    e.i32_const(5);
                    let mut e = e.funcs(ElemKind::FuncRef);
                    e.push(func);
                }

                f.finish()
            },
            ..Default::default()
        }
    });
}

#[test]
#[cfg(miri)]
fn test_take_and_forget() {
    struct Test<'a> {
        value: &'a mut u8,
    }

    let mut b = 0;
    let test = Test { value: &mut b };

    let res = super::take_field_and_leak(test, |t| ptr::addr_of!(t.value));

    assert_eq!(res, &mut 0);
}
