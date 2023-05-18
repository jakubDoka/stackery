# welcome to stackery

Website highly focused on making dependency hell with a custom programming language `stac`. After simple and abusable login, you can start spamming code snippets, search them and search for other users. Wish you happy stacking. Now starts the language documentation.

# Stac

Stac is an interesting language (probably). It feels like dynamicly typed language when you observe some code, but it has types, its just that type inference is abruptly insane. What really happens when stac compiles is that code is interpreted with types. What we are taking advantage of is that generic code (or polymorfic if you will) is generic in text form, but wery much concrete at runtime. We can abuse this fact, in particular, if code is reachable, its types are known since you cannot accept generic input. Your main function will just newer be generic, it does not make sense.

## Type Interpreter

Types are important. More details about the type of the bytes we know, the easier it is to optimize and assert invariants programically. To build a good type system, we need a good axioms. For the language, axions are following:

```grammar
list(P, S) = [ P { S P } [ S ] ]

name = '([a-zA-Z_][a-zA-Z0-9-]*|\`[^\`]\`)'

type {
    self = integer | object | enum | array | pointer

    integer = signed | unsigned | float

    // the bit width is the exponent of 2
    signed   = 'i[3-7]' | 'int'
    unsigned = 'u[3-7]' | 'uint'
    float    = 'f[56]'  | 'float'

    object = '*{' list(name ':' type, ',') '}'
              | '*(' list(type, ',') ')'
    enum   = '|{' list(name ':' type, ',') '}'
              | '|(' list(type, ',') ')'
    array  = '[' type [ ';' expr ] ']'

    pointer = '^' type ['::' type ]
}
```

If you are familiare with Rust, you can recognise that axioms are quite similar, actually, the same. All of these types are builtin into language. Each of thes has its literal syntax so lets look at it:

```grammar
// $() stands for pattern reuse
expr {
    self = bimary_expr
         | unary_expr 

    binary_expr = expr binary_op expr
    unary_expr  = object
                | enum
                | array
                | pointer
                | integer
                | name
                | unary_op unary_expr
                | '(' expr ')'

    integer = unsigned
            | signed 
            | floating_point

    unsigned       = '(0b[01_]+|0o[0-7_]+|[0-9_]+|0x[0-9a-fA-F_]+)(w[3-7])?'
    signed         = '(-)?$(integer.unsigned)'
    floating_point = '(-)?[0-9].[0-9]*(E$(integer.signed))?(w[56])?'
    
    
    object = '*{' list(name [ ':' expr ], ',') '}'
           | '*(' list(epxr, ',') '}'
    enum   = '|{' name [ ':' expr ] '}'
           | '|(' expr ')'
    array  = '[' list(expr, ',') ']'
           | '[' expr ';' expr ']'
    
    pointer = '^' expr [ '::' expr ]

    binary_op = '([*\/+\-=&^|]|>>|<<)(=)?'
    unary_op  = '-' | '!'
}
```

