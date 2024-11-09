let rec f = fun x -> if x = 0 then 1 else f (x - 1) in ...f... -> Let("f", Fun("x", If(Bop(...), Num(1), App(Var("f"), ...))), ...App(f, ...)...)
let rec f = fun x -> if x = 0 then 1 else f (x - 1) in ...f... -> Let("f", Fun("self" Fun("x", If(Bop(...), Num(1), App(Var("self"), ...)))), ...App(f, ...)...)

let rec f = fun x -> if x = 0 then 1 else f (x - 1) in ...

LET REC VAR(fact) EQL FUN VAR(x) IF ... THEN ... ELSE ...f... IN ...f...
                                                      Var(f) Bop(sub, Var(x), Num(1))



let rec f = fun x -> f x
let rec f = fun rec -> fun x -> rec x

let rec g = fun a ->
    let rec f = fun rec -> fun x -> rec x
    in
    g f a
in
g 8

let rec g = fun rec -> fun a ->
    let rec f = fun rec -> fun x -> rec x
    in
    rec f a
in
g 8



   APP
    ├─FUNC OF factorial
    │  └─FUNC OF n
    │     └─IF
    │        ├─BOP(<=)
    │        │  ├─VAR n
    │        │  └─NUM 0
    │       THEN
    │        ├─NUM 1
    │       ELSE
    │        └─BOP(*)
    │           ├─VAR n
    │           └─APP
    │              ├─VAR factorial
    │              └─BOP(-)
    │                 ├─VAR n
    │                 └─NUM 1
    └─APP
       ├─FUNC OF g
       │  └─APP
       │     ├─FUNC OF fact
       │     │  └─FUNC OF n
       │     │     └─IF
       │     │        ├─BOP(<=)
       │     │        │  ├─VAR n
       │     │        │  └─NUM 0
       │     │       THEN
       │     │        ├─NUM 1
       │     │       ELSE
       │     │        └─BOP(*)
       │     │           ├─VAR n
       │     │           └─APP
       │     │              ├─VAR fact
       │     │              └─BOP(-)
       │     │                 ├─VAR n
       │     │                 └─NUM 1
       │     └─APP
       │        ├─VAR g
       │        └─VAR g
       └─FUNC OF g
          └─APP
             ├─FUNC OF fact
             │  └─FUNC OF n
             │     └─IF
             │        ├─BOP(<=)
             │        │  ├─VAR n
             │        │  └─NUM 0
             │       THEN
             │        ├─NUM 1
             │       ELSE
             │        └─BOP(*)
             │           ├─VAR n
             │           └─APP
             │              ├─VAR fact
             │              └─BOP(-)
             │                 ├─VAR n
             │                 └─NUM 1
             └─APP
                ├─VAR g
                └─VAR g