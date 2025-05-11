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
  module Flatten = struct 
      let rec add = function
        | Add (a, b) -> add a @ add b
        | e -> [ e ]
      ;;

      let rec sub = function
        | Sub (a, b) -> sub a @ sub b
        | e -> [ e ]
      ;;

      let rec mul = function
        | Mul (a, b) -> mul a @ mul b
        | e -> [ e ]
      ;;

      let rec exp = function
        | Exp (a, b) -> [ a; b ]
        | e -> [ e ]
      ;;
  end

  let simplify_exp terms =
    match terms with
    | [ e1; Const 1. ] -> e1 (* x^1 = x *)
    | [ _; Const 0. ] -> Const 1. (* x^0 = 1. *)
    | [ Const 0.; _ ] -> Const 0. (* 0.^x = 0. (for x ≠ 0., assuming that) *)
    | [ Const c; Const c2 ] -> Const (c ** c2) (* 0.^x = 0. (for x ≠ 0., assuming that) *)
    | [ e1; e2 ] -> Exp (e1, e2)
    | _ -> failwith "Invalid exponent structure"
  ;;

  let simplify_bina f terms =
    let const_sum, var_terms, others =
      List.fold_left
        (fun (acc_c, acc_v, acc_o) -> function
           | Const c -> f acc_c c, acc_v, acc_o
           | Var v -> acc_c, (v, 1.) :: acc_v, acc_o
           | Mul (Const c, Var v) | Mul (Var v, Const c) -> acc_c, (v, c) :: acc_v, acc_o
           | e -> acc_c, acc_v, e :: acc_o)
        (0., [], [])
        terms
    in
    let grouped_vars =
      List.fold_left
        (fun acc (v, c) ->
           if v = ""
           then acc
           else (
             let prev =
               try List.assoc v acc with
               | Not_found -> 0.
             in
             (v, prev +. c) :: List.remove_assoc v acc))
        []
        var_terms
    in
    let exprs =
      (if const_sum <> 0. then [ Const const_sum ] else [])
      @ List.map
          (fun (v, c) -> if c = 1. then Var v else Mul (Const c, Var v))
          grouped_vars
      @ others
    in
    match exprs with
    | [] -> Const 0.
    | [ e ] -> e
    | hd :: tl -> List.fold_left (fun acc e -> Add (acc, e)) hd tl
  ;;

  let simplify_mul terms =
    let rec process acc_c acc_v acc_other = function
      | [] -> acc_c, acc_v, acc_other
      | Const 0. :: _ -> 0., [], [] (* short-circuit *)
      | Const 1. :: rest -> process acc_c acc_v acc_other rest
      | Const c :: rest -> process (acc_c *. c) acc_v acc_other rest
      | Var v :: rest -> process acc_c (v :: acc_v) acc_other rest
      | e :: rest -> process acc_c acc_v (e :: acc_other) rest
    in
    let const_product, var_list, others = process 1. [] [] terms in
    if const_product = 0.
    then Const 0.
    else (
      let exprs =
        (if const_product <> 1. then [ Const const_product ] else [])
        @ List.map (fun v -> Var v) var_list
        @ others
      in
      match exprs with
      | [] -> Const 1.
      | [ e ] -> e
      | x :: xs -> List.fold_left (fun acc e -> Mul (acc, e)) x xs)
  ;;

  let rec simplify e =
    match e with
    | Const c -> Const c
    | Var v -> Var v
    | Add (_, _) ->
      let flat = Flatten.add e |> List.map simplify in
      simplify_bina ( +. ) flat 
    | Mul (_, _) ->
      let flat = Flatten.mul e |> List.map simplify in
      simplify_mul flat
    | Exp (_, _) ->
      let flat = Flatten.exp e |> List.map simplify in
      simplify_exp flat
    | _ -> e
  ;;

  let post_process_integral flag (expr : expression) =
    flag := false;
    match !flag with
    | true -> Add (expr, Var "c")
    | false -> expr
  ;;
end
