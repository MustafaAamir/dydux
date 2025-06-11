# Dydux 
Under development. A mathematical inference engine in OCaml

# Usage
1. parenthesize expresions when differentiating and integrating
2. E isn't a value, it's a special variable. to use the value of e, use `ev` instead.
3. pi is a hardcoded constant. `pi`
4. support for Ln, sin, cos, tan, diff, integrate, variables, constants (scientific notation)
5. Differentiating goes on inside the derivative engine. 
6. integral engine can call `derivative_engine` and itself (for integration by parts)
7. If you want to define expressions on startup, navigate to the bottom of `expr.ml` and define functions in it using let bindings. `p "let x = "sin(pi)"`
8. I don't rmr if i added evaluating for trig functions. that's not the goal. it's an algebraic engine, not a scientific calculator.


```shell
opam install ppx_deriving core_bench
```
in the root directory:
```shell
$ dune exec dydux

> diff (sin(x)) wrt x
> integrate (sin(x)) wrt x
```

no need for this anymore, just run the repl in bin

1. haven't implemented limits yet but the blueprint is there. 

#### LaTeX output
```shell
> diff (sin(x)) wrt x
> #toggle_latex

```

## breakdown




### Todo 

1. parser grammar that automatically handles precedence

```bnf
AddSub   ::= <MulDiv> ("+" | "-") <AddSub> | <MulDiv>;
MulDiv   ::= <Brackets> ("*" | "/") <MulDiv> | <Brackets>;
Brackets ::= "(" <AddSub> ")" | <Decimal>;
Decimal  ::= <Integer> "." <Integer> | <Integer>;
Integer  ::= <Digit> <Integer> | <Digit>;
Digit    ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9";
```
2. using s-expressions



rulebasedintegration.org/integrationRules.html

rewrite engine

1. Variant functions for different ranges of input values like

let f(x) = 3x + 1 when x < 5 | x ^ 2 when x > 5 
{ 
    encodes rules for the input arguments so if x = 5, returns a domain error in the function definition and prints the rules
}
let f(x, y) ::
    | 3x + 2y^2 when y >= 100
    | 3x + y when y < 100
{
    when no rule is provided, no constraint is set. 
    rule collisions can't occur there has to be a resolver. 
}

let g(x) :: sin(x)^2 + x
let f(x) ::
    | 10x when g(x) = 1
    | x^2
{
    if g(x) is a linear function then the result of f(x) will be
    a parabola with one anomalous point at x = 1
}

let value ::
  let f(x) :: x^2 when x >= 0 and x <= 100 in
  f(100)
{
    evaluates f(x), applies it to 100, binds the result to value, and f(x) 
    goes out of scope
}

let f(x) ::
    | 10x when (let g(x <anonymous arugment>) :: sin(x)^2 + x in g(x)) = 1
    | x^2

{
    types are finicky. I will implement types for matrices to avoid incorrect operations.
}

