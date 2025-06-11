(* traverse expression tree *)
let one = Types.Const 1.
let zero = Types.Const 0.
let integral_flag = ref false

module Engine = struct
  open Types
  open Expr
  open Printer

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
      match (expression : expression) with
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
      | Cos xpr -> Mul (derivative_engine xpr wrt, Mul (Const (-1.), Sin xpr)) |> simplify
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
      | Add (Var m, Mul (Const c, Var n)) when m = n -> Mul (Const (c +. 1.), Var m)
      | Add (Var m, Mul (Var n, Const c)) when m = n -> Mul (Const (c +. 1.), Var m)
      | Add (Mul (Const c, Var n), Var m) when m = n -> Mul (Const (c +. 1.), Var m)
      | Add (Mul (Var n, Const c), Var m) when m = n -> Mul (Const (c +. 1.), Var m)
      (* commutative *)
      | Add (Const m, Add (Const n, x)) -> Add (x, Const (m +. n))
      | Add (Const m, Add (x, Const n)) -> Add (x, Const (m +. n))
      | Add (Add (Const n, x), Const m) -> Add (x, Const (m +. n))
      | Add (Add (x, Const n), Const m) -> Add (x, Const (m +. n))
      (* distributive *)
      | Mul (Var m, Var n) when m = n -> Exp (Var m, Const 2.)
      | Mul (Var m, Exp (Var n, Const c)) when m = n -> Exp (Var m, Const (c +. 1.))
      | Mul (Exp (Var n, Const c), Var m) when m = n -> Exp (Var m, Const (c +. 1.))
      | Mul (x, y) when x = y -> Exp (x, Const 2.)
      | Add (Var m, Var n) when m = n -> Mul (Const 2., Var m)
      | Add (Const 0., x) | Add (x, Const 0.) -> x
      | Sub (Const 0., x) -> Mul (x, Const (-1.))
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
      | Arcsin (Const c) -> Const (asin c)
      | Arccos (Const c) -> Const (acos c)
      | Arctan (Const c) -> Const (atan c)
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
    | Arctan e -> Tan (simplify e) |> simplify'
    | Arccos e -> Arccos (simplify e) |> simplify'
    | Arcsin e -> Arcsin (simplify e) |> simplify'
    | Sec e -> Sec (simplify e) |> simplify'
    | Cosec e -> Cosec (simplify e) |> simplify'
    | Cot e -> Cot (simplify e) |> simplify'
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
    | Let (v, args, e) ->
      let e' = simplify e in
      (match args with
       | Some a ->
         Hashtbl.add ctx v e';
         e'
       | None ->
         Hashtbl.add ctx v e';
         e')
    | _ -> expr
  ;;

  let fixpoint e =
    let rec fixpoint' e (c : int) =
      Printf.printf "%s: %s\n" (string_of_int c) (P.print e);
      let e' = simplify e in
      match e' = e with
      | true -> e'
      | false -> fixpoint' e' (c + 1)
    in
    fixpoint' e 0
  ;;

  let post_process_integral flag (expr : expression) =
    flag := false;
    match !flag with
    | true -> Add (expr, Var "c")
    | false -> expr
  ;;
end

module Generate = struct
  open Types
  open Printer

  let binary_ops = [ "+"; "-"; "*" ]
  let unary_ops = [ "sin"; "cos"; "tan"; "ln" ]
  let random_var vars = List.nth vars (Random.int (List.length vars))
  let random_const () = Random.float 10.0 (* Random float between -10 and 10 *)

  let rec gen max_depth vars =
    if max_depth <= 0
    then
      (* Base case: generate leaf nodes *)
      if Random.bool () then Var (random_var vars) else Const (random_const ())
    else (
      (* Recursive case: generate operations *)
      let choice = Random.int 2 in
      match choice with
      | 0 ->
        (* Binary operations *)
        let op = List.nth binary_ops (Random.int (List.length binary_ops)) in
        let left = gen (max_depth - 1) vars in
        let right = gen (max_depth - 1) vars in
        (match op with
         | "+" -> Add (left, right)
         | "-" -> Sub (left, right)
         | "*" -> Mul (left, right)
         | "/" -> Div (left, right)
         | "^" -> Exp (left, right)
         | _ -> failwith "Invalid binary op")
      | 1 -> if Random.bool () then Var (random_var vars) else Const (random_const ())
      | 2 ->
        (* Unary operations *)
        let op = List.nth unary_ops (Random.int (List.length unary_ops)) in
        let expr = gen (max_depth - 1) vars in
        (match op with
         | "sin" -> Sin expr
         | "cos" -> Cos expr
         | "tan" -> Tan expr
         | "sec" -> Sec expr
         | "cosec" -> Cosec expr
         | "cot" -> Cot expr
         | "arcsin" -> Arcsin expr
         | "arccos" -> Arccos expr
         | "arctan" -> Arctan expr
         | "ln" -> Ln expr
         | _ -> failwith "Invalid unary op")
      | _ ->
        (* Special cases *)
        if Random.bool ()
        then E
        else (
          let base = gen (max_depth - 1) vars in
          let exp = gen (max_depth - 1) vars in
          Log (base, exp)))
  ;;

  let generate ?(max_depth = 3) ?(vars = [ "x"; "y"; "z" ]) () = gen max_depth vars

  let make =
    Random.self_init ();
    let expr1 = generate ~max_depth:10 () in
    Engine.fixpoint expr1 |> P.print |> print_endline;
    let expr2 = generate ~max_depth:10 ~vars:[ "a"; "b"; "c"; "t" ] () in
    Engine.fixpoint expr2 |> P.print |> print_endline
  ;;
end

let () =
  Generate.gen 10 [ "x"; "y"; "z"; "a" ]
  |> Engine.fixpoint
  |> Printer.P.print
  |> print_endline
;;
