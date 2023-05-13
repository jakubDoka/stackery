use crate::*;
gen_instrs! {
	one_bytes: [
		0x00 => Unreachable
		0x01 => Nop
		0x0F => Return
		0xD1 => Refisnull
		0x1A => Drop
		0x1B => Select
		0x45 => I32Eqz
		0x46 => I32Eq
		0x47 => I32Ne
		0x48 => I32LtS
		0x49 => I32LtU
		0x4A => I32GtS
		0x4B => I32GtU
		0x4C => I32LeS
		0x4D => I32LeU
		0x4E => I32GeS
		0x4F => I32GeU
		0x50 => I64Eqz
		0x51 => I64Eq
		0x52 => I64Ne
		0x53 => I64LtS
		0x54 => I64LtU
		0x55 => I64GtS
		0x56 => I64GtU
		0x57 => I64LeS
		0x58 => I64LeU
		0x59 => I64GeS
		0x5A => I64GeU
		0x5B => F32Eq
		0x5C => F32Ne
		0x5D => F32Lt
		0x5E => F32Gt
		0x5F => F32Le
		0x60 => F32Ge
		0x61 => F64Eq
		0x62 => F64Ne
		0x63 => F64Lt
		0x64 => F64Gt
		0x65 => F64Le
		0x66 => F64Ge
		0x67 => I32Clz
		0x68 => I32Ctz
		0x69 => I32Popcnt
		0x6A => I32Add
		0x6B => I32Sub
		0x6C => I32Mul
		0x6D => I32DivS
		0x6E => I32DivU
		0x6F => I32RemS
		0x70 => I32RemU
		0x71 => I32And
		0x72 => I32Or
		0x73 => I32Xor
		0x74 => I32Shl
		0x75 => I32ShrS
		0x76 => I32ShrU
		0x77 => I32Rotl
		0x78 => I32Rotr
		0x79 => I64Clz
		0x7A => I64Ctz
		0x7B => I64Popcnt
		0x7C => I64Add
		0x7D => I64Sub
		0x7E => I64Mul
		0x7F => I64DivS
		0x80 => I64DivU
		0x81 => I64RemS
		0x82 => I64RemU
		0x83 => I64And
		0x84 => I64Or
		0x85 => I64Xor
		0x86 => I64Shl
		0x87 => I64ShrS
		0x88 => I64ShrU
		0x89 => I64Rotl
		0x8A => I64Rotr
		0x8B => F32Abs
		0x8C => F32Neg
		0x8D => F32Ceil
		0x8E => F32Floor
		0x8F => F32Trunc
		0x90 => F32Nearest
		0x91 => F32Sqrt
		0x92 => F32Add
		0x93 => F32Sub
		0x94 => F32Mul
		0x95 => F32Div
		0x96 => F32Fmin
		0x97 => F32Fmax
		0x98 => F32Copysign
		0x99 => F64Abs
		0x9A => F64Neg
		0x9B => F64Ceil
		0x9C => F64Floor
		0x9D => F64Trunc
		0x9E => F64Nearest
		0x9F => F64Sqrt
		0xA0 => F64Add
		0xA1 => F64Sub
		0xA2 => F64Mul
		0xA3 => F64Div
		0xA4 => F64Fmin
		0xA5 => F64Fmax
		0xA6 => F64Copysign
		0xA7 => I32WrapI64
		0xA8 => I32TruncF32S
		0xA9 => I32TruncF32U
		0xAA => I32TruncF64S
		0xAB => I32TruncF64U
		0xAC => I64ExtendI32S
		0xAD => I64ExtendI32U
		0xAE => I64TruncF32S
		0xAF => I64TruncF32U
		0xB0 => I64TruncF64S
		0xB1 => I64TruncF64U
		0xB2 => F32ConvertI32S
		0xB3 => F32ConvertI32U
		0xB4 => F32ConvertI64S
		0xB5 => F32ConvertI64U
		0xB6 => F32DemoteF64
		0xB7 => F64ConvertI32S
		0xB8 => F64ConvertI32U
		0xB9 => F64ConvertI64S
		0xBA => F64ConvertI64U
		0xBB => F64PromoteF32
		0xBC => I32ReinterpretF32
		0xBD => I64ReinterpretF64
		0xBE => F32ReinterpretI32
		0xBF => F64ReinterpretI64
		0xC0 => I32Extend8S
		0xC1 => I32Extend16S
		0xC2 => I64Extend8S
		0xC3 => I64Extend16S
		0xC4 => I64Extend32S
	],
	multy_bytes: [
		0x3F 00 => Memorysize
		0x40 00 => Memorygrow
		0xFC 0 => I32TruncSatF32S
		0xFC 1 => I32TruncSatF32U
		0xFC 2 => I32TruncSatF64S
		0xFC 3 => I32TruncSatF64U
		0xFC 4 => I64TruncSatF32S
		0xFC 5 => I64TruncSatF32U
		0xFC 6 => I64TruncSatF64S
		0xFC 7 => I64TruncSatF64U
		0xFD 14 => I8x16Swizzle
		0xFD 15 => I8x16Splat
		0xFD 16 => I16x8Splat
		0xFD 17 => I32x4Splat
		0xFD 18 => I64x2Splat
		0xFD 19 => F32x4Splat
		0xFD 20 => F64x2Splat
		0xFD 35 => I8x16Veq
		0xFD 36 => I8x16Vne
		0xFD 37 => I8x16VltS
		0xFD 38 => I8x16VltU
		0xFD 39 => I8x16VgtS
		0xFD 40 => I8x16VgtU
		0xFD 41 => I8x16VleS
		0xFD 42 => I8x16VleU
		0xFD 43 => I8x16VgeS
		0xFD 44 => I8x16VgeU
		0xFD 45 => I16x8Veq
		0xFD 46 => I16x8Vne
		0xFD 47 => I16x8VltS
		0xFD 48 => I16x8VltU
		0xFD 49 => I16x8VgtS
		0xFD 50 => I16x8VgtU
		0xFD 51 => I16x8VleS
		0xFD 52 => I16x8VleU
		0xFD 53 => I16x8VgeS
		0xFD 54 => I16x8VgeU
		0xFD 55 => I32x4Veq
		0xFD 56 => I32x4Vne
		0xFD 57 => I32x4VltS
		0xFD 58 => I32x4VltU
		0xFD 59 => I32x4VgtS
		0xFD 60 => I32x4VgtU
		0xFD 61 => I32x4VleS
		0xFD 62 => I32x4VleU
		0xFD 63 => I32x4VgeS
		0xFD 64 => I32x4VgeU
		0xFD 214 => I64x2Veq
		0xFD 215 => I64x2Vne
		0xFD 216 => I64x2VltS
		0xFD 217 => I64x2VgtS
		0xFD 218 => I64x2VleS
		0xFD 219 => I64x2VgeS
		0xFD 65 => F32x4Veq
		0xFD 66 => F32x4Vne
		0xFD 67 => F32x4Vlt
		0xFD 68 => F32x4Vgt
		0xFD 69 => F32x4Vle
		0xFD 70 => F32x4Vge
		0xFD 71 => F64x2Veq
		0xFD 72 => F64x2Vne
		0xFD 73 => F64x2Vlt
		0xFD 74 => F64x2Vgt
		0xFD 75 => F64x2Vle
		0xFD 76 => F64x2Vge
		0xFD 77 => V128Vnot
		0xFD 78 => V128Vand
		0xFD 79 => V128Vandnot
		0xFD 80 => V128Vor
		0xFD 81 => V128Vxor
		0xFD 82 => V128Bitselect
		0xFD 83 => V128Anytrue
		0xFD 96 => I8x16Vabs
		0xFD 97 => I8x16Vneg
		0xFD 98 => I8x16Vpopcnt
		0xFD 99 => I8x16Alltrue
		0xFD 100 => I8x16Bitmask
		0xFD 101 => I8x16NarrowI16x8S
		0xFD 102 => I8x16NarrowI16x8U
		0xFD 107 => I8x16Vshl
		0xFD 108 => I8x16VshrS
		0xFD 109 => I8x16VshrU
		0xFD 110 => I8x16Vadd
		0xFD 111 => I8x16VaddSatS
		0xFD 112 => I8x16VaddSatU
		0xFD 113 => I8x16Vsub
		0xFD 114 => I8x16VsubSatS
		0xFD 115 => I8x16VsubSatU
		0xFD 118 => I8x16VminS
		0xFD 119 => I8x16VminU
		0xFD 120 => I8x16VmaxS
		0xFD 121 => I8x16VmaxU
		0xFD 123 => I8x16AvgrU
		0xFD 124 => I16x8ExtaddpairwiseI8x16S
		0xFD 125 => I16x8ExtaddpairwiseI8x16U
		0xFD 128 => I16x8Vabs
		0xFD 129 => I16x8Vneg
		0xFD 130 => I16x8Q15mulrsatS
		0xFD 131 => I16x8Alltrue
		0xFD 132 => I16x8Bitmask
		0xFD 133 => I16x8NarrowI32x4S
		0xFD 134 => I16x8NarrowI32x4U
		0xFD 135 => I16x8VextendLowI8x16S
		0xFD 136 => I16x8VextendHighI8x16S
		0xFD 137 => I16x8VextendLowI8x16U
		0xFD 138 => I16x8VextendHighI8x16U
		0xFD 139 => I16x8Vshl
		0xFD 140 => I16x8VshrS
		0xFD 141 => I16x8VshrU
		0xFD 142 => I16x8Vadd
		0xFD 143 => I16x8VaddSatS
		0xFD 144 => I16x8VaddSatU
		0xFD 145 => I16x8Vsub
		0xFD 146 => I16x8VsubSatS
		0xFD 147 => I16x8VsubSatU
		0xFD 149 => I16x8Vmul
		0xFD 150 => I16x8VminS
		0xFD 151 => I16x8VminU
		0xFD 152 => I16x8VmaxS
		0xFD 153 => I16x8VmaxU
		0xFD 155 => I16x8AvgrU
		0xFD 156 => I16x8ExtmulLowI8x16S
		0xFD 157 => I16x8ExtmulHighI8x16S
		0xFD 158 => I16x8ExtmulLowI8x16U
		0xFD 159 => I16x8ExtmulHighI8x16U

		0xFD 126 => I32x4ExtaddpairwiseI16x8S
		0xFD 127 => I32x4ExtaddpairwiseI16x8U
		0xFD 160 => I32x4Vabs
		0xFD 161 => I32x4Vneg
		0xFD 163 => I32x4Alltrue
		0xFD 164 => I32x4Bitmask
		0xFD 167 => I32x4VextendLowI16x8S
		0xFD 168 => I32x4VextendHighI16x8S
		0xFD 169 => I32x4VextendLowI16x8U
		0xFD 170 => I32x4VextendHighI16x8U
		0xFD 171 => I32x4Vshl
		0xFD 172 => I32x4VshrS
		0xFD 173 => I32x4VshrU
		0xFD 174 => I32x4Vadd
		0xFD 177 => I32x4Vsub
		0xFD 181 => I32x4Vmul
		0xFD 182 => I32x4VminS
		0xFD 183 => I32x4VminU
		0xFD 184 => I32x4VmaxS
		0xFD 185 => I32x4VmaxU
		0xFD 186 => I32x4DotI16x8S
		0xFD 188 => I32x4ExtmulLowI16x8S
		0xFD 189 => I32x4ExtmulHighI16x8S
		0xFD 190 => I32x4ExtmulLowI16x8U
		0xFD 191 => I32x4ExtmulHighI16x8U

		0xFD 192 => I64x2Vabs
		0xFD 193 => I64x2Vneg
		0xFD 195 => I64x2Alltrue
		0xFD 196 => I64x2Bitmask
		0xFD 199 => I64x2VextendLowI32x4S
		0xFD 200 => I64x2VextendHighI32x4S
		0xFD 201 => I64x2VextendLowI32x4U
		0xFD 202 => I64x2VextendHighI32x4U
		0xFD 203 => I64x2Vshl
		0xFD 204 => I64x2VshrS
		0xFD 205 => I64x2VshrU
		0xFD 206 => I64x2Vadd
		0xFD 209 => I64x2Vsub
		0xFD 213 => I64x2Vmul
		0xFD 220 => I64x2ExtmulLowI32x4S
		0xFD 221 => I64x2ExtmulHighI32x4S
		0xFD 222 => I64x2ExtmulLowI32x4U
		0xFD 223 => I64x2ExtmulHighI32x4U

		0xFD 103 => F32x4Vceil
		0xFD 104 => F32x4Vfloor
		0xFD 105 => F32x4Vtrunc
		0xFD 106 => F32x4Vnearest
		0xFD 224 => F32x4Vabs
		0xFD 225 => F32x4Vneg
		0xFD 227 => F32x4Vsqrt
		0xFD 228 => F32x4Vadd
		0xFD 229 => F32x4Vsub
		0xFD 230 => F32x4Vmul
		0xFD 231 => F32x4Vdiv
		0xFD 232 => F32x4Vmin
		0xFD 233 => F32x4Vmax
		0xFD 234 => F32x4Vpmin
		0xFD 235 => F32x4Vpmax
		0xFD 116 => F64x2Vceil
		0xFD 117 => F64x2Vfloor
		0xFD 122 => F64x2Vtrunc
		0xFD 148 => F64x2Vnearest
		0xFD 236 => F64x2Vabs
		0xFD 237 => F64x2Vneg
		0xFD 239 => F64x2Vsqrt
		0xFD 240 => F64x2Vadd
		0xFD 241 => F64x2Vsub
		0xFD 242 => F64x2Vmul
		0xFD 243 => F64x2Vdiv
		0xFD 244 => F64x2Vmin
		0xFD 245 => F64x2Vmax
		0xFD 246 => F64x2Vpmin
		0xFD 247 => F64x2Vpmax
		0xFD 248 => I32x4TruncSatF32x4S
		0xFD 249 => I32x4TruncSatF32x4U
		0xFD 250 => F32x4ConvertI32x4S
		0xFD 251 => F32x4ConvertI32x4U
		0xFD 252 => I32x4VtruncSatF64x2SZero
		0xFD 253 => I32x4VtruncSatF64x2UZero
		0xFD 254 => F64x2VconvertLowI32x4S
		0xFD 255 => F64x2VconvertLowI32x4U
		0xFD 94 => F32x4VdemoteF64x2Zero
		0xFD 95 => F64x2VpromoteLowF32x4

	],
	others: [
		0x0C => br(l: Labelidx) { l }
		0x0D => brif(l: Labelidx) { l }
		0x10 => call(x: Funcidx) { x }
		0x11 => callindirect(y: Typeidx, x: Tableidx) { y x }
		0xD0 => refnull(t: Reftype) { t }
		0xD2 => reffunc(x: Funcidx) { x }
		0x20 => localget(x: Localidx) { x }
		0x21 => localset(x: Localidx) { x }
		0x22 => localtee(x: Localidx) { x }
		0x23 => globalget(x: Globalidx) { x }
		0x24 => globalset(x: Globalidx) { x }
		0x25 => tableget(x: Tableidx) { x }
		0x26 => tableset(x: Tableidx) { x }
		0xFC => tableinit(y: Elemidx, x: Tableidx) { 12 y x }
		0xFC => elemdrop(x: Elemidx) { 13 x }
		0xFC => tablecopy(x: Tableidx, y: Tableidx) { 14 x y }
		0xFC => tablegrow(x: Tableidx) { 15 x }
		0xFC => tablesize(x: Tableidx) { 16 x }
		0xFC => tablefill(x: Tableidx) { 17 x }
		0x28 => i32_load(m: Memarg) { m }
		0x29 => i64_load(m: Memarg) { m }
		0x2A => f32_load(m: Memarg) { m }
		0x2B => f64_load(m: Memarg) { m }
		0x2C => i32_load8_s(m: Memarg) { m }
		0x2D => i32_load8_u(m: Memarg) { m }
		0x2E => i32_load16_s(m: Memarg) { m }
		0x2F => i32_load16_u(m: Memarg) { m }
		0x30 => i64_load8_s(m: Memarg) { m }
		0x31 => i64_load8_u(m: Memarg) { m }
		0x32 => i64_load16_s(m: Memarg) { m }
		0x33 => i64_load16_u(m: Memarg) { m }
		0x34 => i64_load32_s(m: Memarg) { m }
		0x35 => i64_load32_u(m: Memarg) { m }
		0x36 => i32_store(m: Memarg) { m }
		0x37 => i64_store(m: Memarg) { m }
		0x38 => f32_store(m: Memarg) { m }
		0x39 => f64_store(m: Memarg) { m }
		0x3A => i32_store8(m: Memarg) { m }
		0x3B => i32_store16(m: Memarg) { m }
		0x3C => i64_store8(m: Memarg) { m }
		0x3D => i64_store16(m: Memarg) { m }
		0x3E => i64_store32(m: Memarg) { m }
		0xFC => memoryinit(x: Dataidx) { 8 x 00 }
		0xFC => datadrop(x: Dataidx) { 9 x }
		0xFC => memorycopy() { 10 00 00 }
		0xFC => memoryfill() { 11 00 }
		0x41 => i32_const(n: i32) { n }
		0x42 => i64_const(n: i64) { n }
		0x43 => f32_const(z: f32) { z }
		0x44 => f64_const(z: f64) { z }
		0xFD => v128_load(m: Memarg) { 0 m }
		0xFD => v128_load8x8_s(m: Memarg) { 1 m }
		0xFD => v128_load8x8_u(m: Memarg) { 2 m }
		0xFD => v128_load16x4_s(m: Memarg) { 3 m }
		0xFD => v128_load16x4_u(m: Memarg) { 4 m }
		0xFD => v128_load32x2_s(m: Memarg) { 5 m }
		0xFD => v128_load32x2_u(m: Memarg) { 6 m }
		0xFD => v128_load8_splat(m: Memarg) { 7 m }
		0xFD => v128_load16_splat(m: Memarg) { 8 m }
		0xFD => v128_load32_splat(m: Memarg) { 9 m }
		0xFD => v128_load64_splat(m: Memarg) { 10 m }
		0xFD => v128_load32_zero(m: Memarg) { 92 m }
		0xFD => v128_load64_zero(m: Memarg) { 93 m }
		0xFD => v128_store(m: Memarg) { 11 m }
		0xFD => v128_load8_lane(m: Memarg, l: Laneidx) { 84 m l }
		0xFD => v128_load16_lane(m: Memarg, l: Laneidx) { 85 m l }
		0xFD => v128_load32_lane(m: Memarg, l: Laneidx) { 86 m l }
		0xFD => v128_load64_lane(m: Memarg, l: Laneidx) { 87 m l }
		0xFD => v128_store8_lane(m: Memarg, l: Laneidx) { 88 m l }
		0xFD => v128_store16_lane(m: Memarg, l: Laneidx) { 89 m l }
		0xFD => v128_store32_lane(m: Memarg, l: Laneidx) { 90 m l }
		0xFD => v128_store64_lane(m: Memarg, l: Laneidx) { 91 m l }
		0xFD => i8x16_extractlane_s(l: Laneidx) { 21 l }
		0xFD => i8x16_extractlane_u(l: Laneidx) { 22 l }
		0xFD => i8x16_replacelane(l: Laneidx) { 23 l }
		0xFD => i16x8_extractlane_s(l: Laneidx) { 24 l }
		0xFD => i16x8_extractlane_u(l: Laneidx) { 25 l }
		0xFD => i16x8_replacelane(l: Laneidx) { 26 l }
		0xFD => i32x4_extractlane(l: Laneidx) { 27 l }
		0xFD => i32x4_replacelane(l: Laneidx) { 28 l }
		0xFD => i64x2_extractlane(l: Laneidx) { 29 l }
		0xFD => i64x2_replacelane(l: Laneidx) { 30 l }
		0xFD => f32x4_extractlane(l: Laneidx) { 31 l }
		0xFD => f32x4_replacelane(l: Laneidx) { 32 l }
		0xFD => f64x2_extractlane(l: Laneidx) { 33 l }
		0xFD => f64x2_replacelane(l: Laneidx) { 34 l }
	],
}
// 0x1C Select [("t^\\ast", "vec(\\Bvaltype)")]
// 0xFD V128Vconst [("12", "u32"), ("(b", "byte)^{16}")]
// 0xFD I8x16Shuffle [("13", "u32"), ("(l", "laneidx)^{16}")]
// 0x02 ~~\X{bt}{:}\Bblocktype~~(\X{in}{:}\Binstr)^\ast~~\hex{0B}
// 0x03 ~~\X{bt}{:}\Bblocktype~~(\X{in}{:}\Binstr)^\ast~~\hex{0B}
// 0x04 ~~\X{bt}{:}\Bblocktype~~(\X{in}{:}\Binstr)^\ast~~\hex{0B}
// 0x04 ~~\X{bt}{:}\Bblocktype~~(\X{in}_1{:}\Binstr)^\ast~~
// 0x0E ~~l^\ast{:}\Bvec(\Blabelidx)~~l_N{:}\Blabelidx
