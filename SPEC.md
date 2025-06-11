```mathematica
let f(x) :: 3x + 1 when x < 5 | x ^ 2 when x > 5
```
#=
    encodes rules for the input arguments so if x = 5, returns a 
    domain error in the function definition and prints the rules 
=#

let f(x, α) ::
    | 3x + 2α^2 when α >= 100
    | 3x + α when α < 100
#=
    when no rule is provided, no constraint is set.
    rule collisions can't occur there has to be a resolver.
#=

```
let g(x) :: sin(x)^2 + x
let f(x) ::
    | 10x when g(x) = 1
    | x^2
```
#=
    if g(x) is a linear function then the result of f(x) will be
    a parabola with one anomalous point at x = 1
=#

let value ::
  let f(x) :: x^2 when x >= 0 and x <= 100 in
  f(100)
#=
    evaluates f(x), applies it to 100, binds the result to value, and f(x)
    goes out of scope
=#

let f(x) ::
    | 10x when (let g(x <anonymous arugment>) :: sin(x)^2 + x in g(x)) = 1
    | x^2

let squuare :: [[1, 2], [3, 4]]
#=
    types are finicky. I will implement types for matrices to avoid incorrect operations.
=#

implement a constraint solver using z3
#=
  when x > 1
  when x < 1
  when x = 1
  compile down to the default constraint, which is none

  when x <> 1
  when x = 1
  is a violation
=#

# Z3
```lisp
  (declare-const x Int)
  (declare-const y Int)
  (define-fun a1 () Bool (> x 0))
  (define-fun a2 () Bool (< x y))
  (define-fun a3 () Bool (<= (+ y x) 0))
  (assert (= a3 a1))
  (assert (or a3 a2))
  (assert-soft a3         :weight 3)
  (assert-soft (not a3)   :weight 5)
  (assert-soft (not a1)   :weight 10)
  (assert-soft (not a2)   :weight 3)
  (check-sat)
  (get-model)
  (get-objectives)
  (eval a1)
  (eval a2)
  (eval a3)
```

#=
features
  | [HOT] debug mode, lets you view steps
  | pattern matching on expressions ML style
  |> let a :: x^2
     let b :: 3
     let c :: match (a, b)
     | x^2, 3 => begin x^2 end
     | _, 3 => begin 10.0 end
  | builtin csv reader
    [file] 10,20,30
  |> let a :: csv name seperator
     let b :: map (λx.x^2) a
     val b : 100, 400, 900
  partial function application
  | let f(x, y) :: 2xy
    let a :: f 1
    val a = 2y
  | list of expressions
  | lambdas
  | utf-8 support
  | while (λx.|x|)(i) > 0) (loops forever)
  | while loops
    builtins
    - lazy (doesn't evaluate a the result)
    | let a :: lazy 1 + 2 + f(1)
    - gcd (greatest commond denominator)
    | let a :: gcd (f(1), f(2))
    - factorize
    | let e :: x ^ 2 - 2xy + y^2
      let f :: factorize e
=#
