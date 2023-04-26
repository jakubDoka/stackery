# Stac

Stac is an programming language used in stackery. It is strongly stack oriented with its stack based sintax that is rather unusual but admittedly, it is in fact esoteric lang. The goal of this crate is to offer Stac to wasm compiler and code analizer. In next sections we will go rtough syntax and spec of Stac.

## Imports

The beggining of each stac code can start with `use` segment. The syntax is following:

```rust
	"post"
	"post with complex name" pwcm
use
```

How this works is that strings and identifiers are pushed on stack and by using the keyword you pop them and trun into import node that can then triggers post imports. After each string you can optionally rename the import since name of the post can be whatever you want and that does not nessesarly translate to use in code.

## Comments

Commenst are c-stile

```rust
// line comment
```

## Literals

You can use various constats. (section will incrementally expand in the future)

```rust
0 100 3000 // 32 bit unsigned literal
```
