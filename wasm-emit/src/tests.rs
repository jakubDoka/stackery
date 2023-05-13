use crate::*;
use alloc::vec;
use alloc::vec::Vec;

fn perform_test<'a>(tester: impl FnOnce(Encoder<DefaultBackend>) -> Module<'a>) -> Vec<u8> {
    let mut buffer = vec![];
    let mut integers = vec![];
    let mut emmited: Vec<u8> = vec![];

    let module = tester(Encoder::<DefaultBackend>::new(&mut integers, &mut buffer));
    module
        .emmit(
            Encoder::<DefaultBackend>::new(&mut integers, &mut buffer),
            &mut emmited,
        )
        .unwrap();

    wasmparser::validate(&emmited).unwrap();

    emmited
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
