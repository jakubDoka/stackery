use std::marker::PhantomData;

use crate::Serde128;

use crate as serde_base128;

#[derive(Serde128, Debug, PartialEq, Eq)]
pub struct Unit;

#[derive(Serde128, Debug, PartialEq, Eq)]
pub struct Tuple(u64, i64);

#[derive(Serde128, Debug, PartialEq, Eq)]
pub struct Named {
    a: u64,
    b: i64,
}

#[derive(Serde128, Debug, PartialEq, Eq)]
pub struct Generic<T>
where
    T: Copy,
{
    a: T,
}

#[derive(Serde128, Debug, PartialEq, Eq)]
#[ignore_param(T)]
pub struct Ft<T>(PhantomData<T>);

#[derive(Debug, PartialEq, Eq)]
struct Other(u8);

#[derive(Default)]
struct UseOther;

impl crate::UseSerde128<Other> for UseOther {
    fn serialize(self, _value: &Other, _decoder: &mut crate::Encoder) {
        _decoder.write_byte(_value.0);
    }

    unsafe fn deserialize(self, _decoder: &mut crate::Decoder) -> Other {
        Other(_decoder.read_byte())
    }
}

#[derive(Serde128, Debug, PartialEq, Eq)]
struct UseOtherStruct(#[uses(UseOther)] Other);

#[test]
fn decode_encode() {
    #[derive(Serde128)]
    struct Everything {
        unit: Unit,
        tuple: Tuple,
        named: Named,
        generic: Generic<u64>,
        ft: Ft<u64>,
        #[uses(UseOther)]
        other: Other,
        use_other: UseOtherStruct,
    }

    let everything = Everything {
        unit: Unit,
        tuple: Tuple(1, -1),
        named: Named { a: 2, b: -2 },
        generic: Generic { a: 3 },
        ft: Ft(PhantomData),
        other: Other(4),
        use_other: UseOtherStruct(Other(5)),
    };

    let mut encoder = crate::Encoder::new();
    everything.serialize(&mut encoder);
    let data = encoder.into_vec();

    let mut decoder = crate::Decoder::new(&data);
    let everything = unsafe { Everything::deserialize(&mut decoder) };

    assert_eq!(everything.unit, Unit);
}
