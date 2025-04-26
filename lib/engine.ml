(* traverse expression tree *)

let one = Types.Const 1.
let zero = Types.Const 0.
let integral_flag = ref false

module Engine = struct
  open Types
  open Expr
  open Printer

  type expression_type =
    | Algebraic
    | Exponential
    | Trigonometric
    | Logarithmic
    | Other
  [@@deriving show { with_path = false }]

  let rec subst x s expr =
    match expr with
    | Var v when x = v -> s
    | Mul (e1, e2) -> Mul (subst x s e1, subst x s e2)
    | Div (e1, e2) -> Div (subst x s e1, subst x s e2)
    | Add (e1, e2) -> Add (subst x s e1, subst x s e2)
    | Sub (e1, e2) -> Sub (subst x s e1, subst x s e2)
    | Exp (e1, e2) -> Exp (subst x s e1, subst x s e2)
    | Sin e1 -> Sin (subst x s e1)
    | Ln e1 -> Ln (subst x s e1)
    | Cos e1 -> Cos (subst x s e1)
    | Tan e1 -> Tan (subst x s e1)
    | Integral (e1, wrt, limits) -> Integral (subst x s e1, wrt, limits)
    | _ -> expr
  ;;

  let rec simplify expr =
    let rec derivative_engine expression wrt =
      (*
         (Diff (
   (Mul ((Sin (Add ((Mul ((Const 2.), (Const 3.14159265359))), (Var "x")))),
      (Cos (Add ((Mul ((Const 2.), (Const 3.14159265359))), (Var "x")))))),
   "x"))
      *)
      match expression with
      | Exp (Const c, Var _) -> Mul (expression, Ln (Const c))
      | Mul (E, Var _) | Mul (Var _, E) -> E
      | Exp (E, Var _) -> expression
      | Mul (Const _, E) | Mul (E, Const _) -> Const 0.
      | Exp (E, e1) ->
        let e1' = simplify (derivative_engine e1 wrt) in
        (match e1' with
         | Const x when x = 0.0 -> Const 0.0
         | _ -> Mul (e1', Exp (E, e1)))
      | Const _ | E -> Const 0.0
      | Var x when x = wrt -> Const 1.
      | Var x -> Var x
      | Add (e1, e2) ->
        let e1' = simplify (derivative_engine e1 wrt) in
        let e2' = simplify (derivative_engine e2 wrt) in
        Add (e1', e2')
      | Sub (e1, e2) ->
        let e1' = simplify (derivative_engine e1 wrt) in
        let e2' = simplify (derivative_engine e2 wrt) in
        Sub (e1', e2')
      | Ln e ->
        let e' = simplify (derivative_engine e wrt) in
        Mul (e', Div (one, e) |> simplify) |> simplify
      | Mul (Const c, Var _) | Mul (Var _, Const c) -> Const c
      | Mul (e1, e2) ->
        let e1' = simplify (derivative_engine e1 wrt) in
        let e2' = simplify (derivative_engine e2 wrt) in
        Add (Mul (e1', e2), Mul (e1, e2'))
      | Div (e1, e2) ->
        let e1' = simplify (derivative_engine e1 wrt) in
        let e2' = simplify (derivative_engine e2 wrt) in
        Div
          (Sub (Mul (e1', e2) |> simplify, Mul (e1, e2') |> simplify), Exp (e2, Const 2.))
        |> simplify
      | Exp (Var x, Const c) -> Mul (Const c, simplify (Exp (Var x, Const (c -. 1.))))
      | Exp (e1, Const c) ->
        Mul
          ( Mul (Const c, Exp (e1, Const (c -. 1.))) |> simplify
          , derivative_engine e1 wrt |> simplify )
        |> simplify
      | Cos (Var x) -> Mul (Const (-1.), Sin (Var x)) |> simplify
      | Sin xpr -> Mul (derivative_engine xpr wrt |> simplify, Cos xpr) |> simplify
      | Cos xpr ->
        Mul (derivative_engine xpr wrt, Mul (Const (-1.), Sin xpr))
        |> simplify
      | _ -> failwith "Not implemented yet"
    in
    let rec integral_engine expression wrt limits =
      let res =
        match simplify expression with
        | Var x when x = wrt ->
          let num = Exp (Var x, Const 2.) |> simplify in
          Div (num, Const 2.) |> simplify
        | Exp (Var v, Const c) ->
          let num = Exp (Var v, Const (c +. 1.)) |> simplify in
          Div (num, Const (c +. 1.)) |> simplify
        | Const c -> Mul (Const c, Var wrt)
        | Div (Const c, expr) ->
          Div (Mul (Const c, Ln expr) |> simplify, derivative_engine expr wrt |> simplify)
          |> simplify
        | Mul (Const c, Var v) ->
          Mul (Const c, Div (Exp (Var v, Const 2.) |> simplify, Const 2.)) |> simplify
        | Sin e ->
          let ie = simplify e in
          let dif = derivative_engine ie wrt in
          Mul (Div (Const (-1.), dif) |> simplify, Cos ie) |> simplify
        | Cos e ->
          let ie = simplify e in
          let dif = derivative_engine ie wrt in
          Mul (Div (Const 1., dif) |> simplify, Sin ie) |> simplify
        | Tan e ->
          let ie = simplify e in
          let dif = derivative_engine ie wrt in
          Mul (Div (Const (-1.), dif) |> simplify, Ln (Cos ie)) |> simplify
        | Add (e1, e2) ->
          Add
            ( integral_engine (simplify e1) wrt limits
            , integral_engine (simplify e2) wrt limits )
        | Sub (e1, e2) ->
          Sub
            ( integral_engine (simplify e1) wrt limits
            , integral_engine (simplify e2) wrt limits )
        | _ -> expression |> simplify
      in
      match limits with
      | Some (l1, l2) ->
        integral_flag := false;
        Sub (subst wrt l1 res |> simplify, subst wrt l2 res |> simplify) |> simplify
      | None -> res |> simplify
    in
    let simplify' = function
      | Add (Const m, Const n) -> Const (m +. n)
      | Sub (Const m, Const n) -> Const (m -. n)
      | Mul (Const m, Const n) -> Const (m *. n)
      | Mul (Var m, Var n) when m = n -> Exp (Var m, Const 2.)
      | Add (Var m, Var n) when m = n -> Mul (Const 2., Var m)
      | Add (Const 0., x) | Add (x, Const 0.) -> x
      | Sub (Const 0., x) | Mul (x, Const -1.) -> x
      | Sub (x, Const 0.) -> x
      | Sub (Var m, Var n) when m = n -> Const 0.
      | Mul (Const 0., _) | Mul (_, Const 0.) -> Const 0.
      | Mul (Const 1., x) | Mul (x, Const 1.) -> x
      | Div (Const 0., _) -> Const 0.
      | Div (_, Const 0.) -> "Division by Zero Error" |> failwith
      | Div (Const m, Const n) -> Const (m /. n)
      | Div (x, Const 1.) -> x
      | Div (Var x, Var y) when x = y -> Const 1.
      | Div (Var x, Mul (Const c, Var y)) when x = y -> Const (1. /. c)
      | Div (Mul (Const c, Var x), Var y) when x = y -> Const c
      | Div (Mul (Const a, Var x), Mul (Const b, Var y)) when x = y -> Const (a /. b)
      | Exp (_, Const 0.) | Exp (Const 1., _) -> Const 1.
      | Exp (x, Const 1.) -> x
      | Exp (Const m, Const n) -> Const (m ** n)
      | Sin (Const n) -> Const (sin n)
      | Cos (Const n) -> Const (cos n)
      | Tan (Const n) -> Const (tan n)
      | Ln E -> Const 1.
      | Exp (E, Ln x) -> x
      | Ln (Mul (E, x)) | Ln (Mul (x, E)) -> Add (Const 1., Ln x)
      | Add (Ln x, Ln y) -> Ln (Mul (x, y))
      | Sub (Ln x, Ln y) -> Ln (Div (x, y))
      | Ln (Exp (E, y)) -> y
      | Ln (Exp (x, y)) -> Mul (y, Ln x)
      | _ -> expr
    in
    match expr with
    | Var v ->
      (match Hashtbl.find_opt ctx v with
       | Some e -> simplify' e
       | None -> Var v)
    | Add (e1, e2) -> Add (simplify e1, simplify e2) |> simplify' (* base case*)
    | Sub (e1, e2) -> Sub (simplify e1, simplify e2) |> simplify'
    | Mul (e1, e2) -> Mul (simplify e1, simplify e2) |> simplify'
    | Div (e1, e2) -> Div (simplify e1, simplify e2) |> simplify'
    | Exp (e1, e2) -> Exp (simplify e1, simplify e2) |> simplify'
    | E -> E
    | Sin e -> Sin (simplify e) |> simplify'
    | Cos e -> Cos (simplify e) |> simplify'
    | Tan e -> Tan (simplify e) |> simplify'
    | Ln e -> Ln (simplify e) |> simplify'
    | Diff (expression, x) -> derivative_engine (simplify expression) x |> simplify
    | Const c -> Const c
    | Integral (expression, x, Some (l1, l2)) ->
      integral_flag := true;
      integral_engine (simplify expression) x (Some (simplify l1, simplify l2))
      |> simplify
    | Integral (expression, x, None) ->
      integral_flag := true;
      integral_engine (simplify expression) x None |> simplify
    | Let (v, e) ->
      let e' = simplify e in
      Hashtbl.add ctx v e';
      e'
  ;;

  let post_process_integral flag (expr : expression) =
    flag := false;
    match !flag with
    | true -> Add (expr, Var "c")
    | false -> expr
  ;;
end
