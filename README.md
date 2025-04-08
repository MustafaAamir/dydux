# matheatrics
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


cd into lib/ and do this
```shell
> utop
> #use "expr.ml";;
> p "diff (sin(x)) wrt x";;
> p "integrate (sin(x)) wrt x";;
```

no need for this anymore, just run the repl in bin

1. haven't implemented limits yet but the blueprint is there. 

#### LaTeX output
```shell
> pl "diff (sin(x)) wrt x"

UPDATE: 
use `#toggle_latex`

```


# TODO

I'll create a repl to avoid utop


