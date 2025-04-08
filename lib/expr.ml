(*
   TODO: 
    0.5. integration ffs
    0.75. integrate e
    1. add support for ln
    2.
*)

type expression =
  | Var of string
  | Const of float
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Exp of expression * expression
  | Sin of expression
  | Cos of expression
  | Tan of expression
  | Diff of expression * string
  | E
  | Ln of expression
  | Let of string * expression
  | Integral of expression * string * (float * float) option

let pi = 3.1415926535897932384626433
let e = 2.718281828459045235360287471352
let one = Const 1.
let zero = Const 0.

type ctxt = (string, expression) Hashtbl.t

let ctx = Hashtbl.create 0

let safe_int_to_string x =
  let diff = abs_float (x -. float_of_int (int_of_float x)) in
  if diff < 1e-9 then string_of_int (int_of_float x) else string_of_float x
;;

let rec prec = function
  | Add _ | Sub _ -> 1
  | Div _ -> 2
  | Mul _ -> 5
  | Exp _ -> 7
  | _ -> 10

and pp_paren parent_prec expr =
  let self_prec = prec expr in
  let s = pp expr in
  if self_prec < parent_prec then "(" ^ s ^ ")" else s

and pp = function
  | E -> "e"
  | Const x -> safe_int_to_string x
  | Var x -> x
  | Add (e1, e2) -> Printf.sprintf "%s + %s" (pp_paren 1 e1) (pp_paren 1 e2)
  | Sub (e1, e2) -> Printf.sprintf "%s - %s" (pp_paren 1 e1) (pp_paren 2 e2)
  | Mul (Const c, Var x) -> Printf.sprintf "%s%s" (pp @@ Const c) (pp @@ Var x)
  | Mul (Const c, e1) -> Printf.sprintf "%s(%s)" (pp @@ Const c) (pp e1)
  | Mul (e1, e2) -> Printf.sprintf "%s * %s" (pp_paren 2 e1) (pp_paren 2 e2)
  | Div (e1, e2) -> Printf.sprintf "%s / %s" (pp_paren 2 e1) (pp_paren 3 e2)
  | Exp (e1, e2) -> Printf.sprintf "%s ^ %s" (pp_paren 3 e1) (pp_paren 4 e2)
  | Sin e1 -> Printf.sprintf "sin(%s)" (pp e1)
  | Cos e1 -> Printf.sprintf "cos(%s)" (pp e1)
  | Tan e1 -> Printf.sprintf "tan(%s)" (pp e1)
  | Ln e1 -> Printf.sprintf "ln(%s)" (pp e1)
  | Diff (expression, x) -> Printf.sprintf "∂%s .wrt %s" (pp expression) x
  | Integral (expression, x, Some limits) ->
    Printf.sprintf
      "∫%s .wrt %s{%s to %s}"
      (pp expression)
      x
      (fst limits |> safe_int_to_string)
      (snd limits |> safe_int_to_string)
  | Integral (expression, x, None) -> Printf.sprintf "∫%s .wrt %s" (pp expression) x
  | Let (var, expr) -> Printf.sprintf "%s = %s" var (pp expr)
;;

(*abstract this away*)
let rec pp_latex_paren parent_prec expr =
  let self_prec = prec expr in
  let s = pp_latex expr in
  if self_prec < parent_prec then "\\left(" ^ s ^ "\\right)" else s

and pp_latex = function
  | E -> "e"
  | Const x -> safe_int_to_string x
  | Var x -> x
  | Add (e1, e2) -> Printf.sprintf "%s + %s" (pp_latex_paren 1 e1) (pp_latex_paren 1 e2)
  | Sub (e1, e2) -> Printf.sprintf "%s - %s" (pp_latex_paren 1 e1) (pp_latex_paren 2 e2)
  | Mul (Const c, Var x) -> Printf.sprintf "%s%s" (pp_latex (Const c)) (pp_latex (Var x))
  | Mul (Const c, e) ->
    Printf.sprintf "%s\\left(%s\\right)" (pp_latex (Const c)) (pp_latex e)
  | Mul (e1, e2) ->
    Printf.sprintf "%s \\cdot %s" (pp_latex_paren 2 e1) (pp_latex_paren 2 e2)
  | Div (e1, e2) -> Printf.sprintf "\\frac{%s}{%s}" (pp_latex e1) (pp_latex e2)
  | Exp (e1, e2) -> Printf.sprintf "%s^{%s}" (pp_latex_paren 3 e1) (pp_latex_paren 4 e2)
  | Sin e -> Printf.sprintf "\\sin\\left(%s\\right)" (pp_latex e)
  | Cos e -> Printf.sprintf "\\cos\\left(%s\\right)" (pp_latex e)
  | Tan e -> Printf.sprintf "\\tan\\left(%s\\right)" (pp_latex e)
  | Ln e -> Printf.sprintf "\\ln\\left(%s\\right)" (pp_latex e)
  | Diff (expression, x) ->
    Printf.sprintf
      "\\frac{\\partial}{\\partial %s} \\left(%s\\right)"
      x
      (pp_latex expression)
  | Integral (expression, x, Some (a, b)) ->
    Printf.sprintf
      "\\int_{%s}^{%s} %s\\, d%s"
      (safe_int_to_string a)
      (safe_int_to_string b)
      (pp_latex expression)
      x
  | Integral (expression, x, None) ->
    Printf.sprintf "\\int %s\\, d%s" (pp_latex expression) x
  | Let (var, expr) -> Printf.sprintf "%s = %s" var (pp_latex expr)
;;

module Lexer = struct
  type token =
    | LParen
    | RParen
    | LBracket
    | Let
    | RBracket
    | LCBracket
    | RCBracket
    | Assign
    | Symbol of char
    | LVar of string
    | Sin
    | Cos
    | Tan
    | Ln
    | E
    | LDiff
    | LIntegral
    | Wrt
    | Comma
    | Nat of float
    | EOF

  type state =
    { input : string
    ; mutable ch : char
    ; mutable pos : int
    ; len : int
    }

  let init inp =
    if inp = "\n"
    then { input = ""; pos = 0; ch = '\x00'; len = 0 }
    else { input = inp; pos = 0; ch = String.get inp 0; len = String.length inp }
  ;;

  let is_alpha c =
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' -> true
    | _ -> false
  ;;

  let is_alphanum c =
    match c with
    | '0' .. '9' -> true
    | _ when is_alpha c -> true
    | _ -> false
  ;;

  let is_num c =
    match c with
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let id st i =
    let j = ref (i + 1) in
    while !j < st.len && is_alphanum st.input.[!j] do
      incr j
    done;
    String.sub st.input i (!j - i), j
  ;;

  (*
     let nat st i =
    let j = ref (i + 1) in
    while !j < st.len && is_num st.input.[!j] do
      incr j
    done;
    String.sub st.input i (!j - i), j
  ;;
  *)
  let nat st i =
    let j = ref (i + 1) in
    let has_decimal = ref false in
    let _start =
      if i < st.len && st.input.[i] = '-'
      then (
        incr j;
        i + 1)
      else i
    in
    (* First check the integer part *)
    while !j < st.len && is_num st.input.[!j] do
      incr j
    done;
    (* Then check for decimal point followed by more numbers *)
    if !j < st.len && st.input.[!j] = '.'
    then (
      has_decimal := true;
      incr j;
      while !j < st.len && is_num st.input.[!j] do
        incr j
      done);
    (* Handle scientific notation like 1.23e-4 *)
    if !j < st.len && (st.input.[!j] = 'e' || st.input.[!j] = 'E')
    then (
      incr j;
      if !j < st.len && (st.input.[!j] = '+' || st.input.[!j] = '-') then incr j;
      while !j < st.len && is_num st.input.[!j] do
        incr j
      done);
    String.sub st.input i (!j - i), j
  ;;

  let lex input =
    let st = init input in
    let symbol_arr =
      [| '~'
       ; '`'
       ; '!'
       ; '@'
       ; '#'
       ; '$'
       ; '%'
       ; '^'
       ; '&'
       ; '*'
       ; '-'
       ; '+'
       ; ';'
       ; '/'
       ; ':'
       ; ';'
       ; '<'
       ; '>'
       ; '.'
       ; '?'
       ; ';'
      |]
    in
    match input with
    | "" -> [ EOF ]
    | s when st.pos < st.len ->
      let rec aux i =
        let advance st i =
          if st.pos + i < st.len
          then (
            st.pos <- st.pos + i;
            st.ch <- String.get st.input st.pos;
            aux st.pos)
          else [ EOF ]
        in
        match st.input.[i] with
        (* needs to match with '-', '+' not that important *)
        | '0' .. '9' ->
          let var, j = nat st st.pos in
          Nat (float_of_string var) :: advance st (!j - st.pos)
        | ' ' | '\t' | '\n' | '\r' -> advance st 1
        | '(' -> LParen :: advance st 1
        | ')' -> RParen :: advance st 1
        | ']' -> RBracket :: advance st 1
        | '[' -> LBracket :: advance st 1
        | '}' -> RCBracket :: advance st 1
        | '{' -> LCBracket :: advance st 1
        | ',' -> Comma :: advance st 1
        | '=' -> Assign :: advance st 1
        | x when Array.mem x symbol_arr -> Symbol x :: advance st 1
        | 'A' .. 'Z' | 'a' .. 'z' | '\'' ->
          let var, j = id st st.pos in
          (match var with
           | "sin" -> Sin :: advance st (!j - st.pos)
           | "cos" -> Cos :: advance st (!j - st.pos)
           | "tan" -> Tan :: advance st (!j - st.pos)
           | "diff" -> LDiff :: advance st (!j - st.pos)
           | "wrt" -> Wrt :: advance st (!j - st.pos)
           | "let" -> Let :: advance st (!j - st.pos)
           | "integrate" -> LIntegral :: advance st (!j - st.pos)
           | "e" -> E :: advance st (!j - st.pos)
           | "ln" -> Ln :: advance st (!j - st.pos)
           | _ -> LVar var :: advance st (!j - st.pos))
        | _ -> "Unexpected character: " ^ String.make 1 s.[i] |> failwith
      in
      aux 0
    | _ -> [ EOF ]
  ;;
end

module Parser = struct
  open Lexer

  let parse tokens =
    let rec parse_add = function
      | [] -> failwith "Empty input"
      | tokens ->
        let left, rest = parse_mul tokens in
        (match rest with
         | Symbol '+' :: rest' ->
           let right, rest'' = parse_add rest' in
           Add (left, right), rest''
         | Symbol '-' :: rest' ->
           let right, rest'' = parse_add rest' in
           Sub (left, right), rest''

         | _ -> left, rest)
    and parse_mul = function
      | [] -> failwith "Empty input"
      | tokens ->
        let left, rest = parse_exp tokens in
        (match rest with
         | Symbol '*' :: rest' ->
           let right, rest'' = parse_mul rest' in
           Mul (left, right), rest''
         | Symbol '/' :: rest' ->
           let right, rest'' = parse_mul rest' in
           Div (left, right), rest''
         | _ -> left, rest)
    and parse_exp = function
      | [] -> failwith "Empty input"
      | tokens ->
        let left, rest = parse_atom tokens in
        (match rest with
         | Symbol '^' :: rest' ->
           let right, rest'' = parse_mul rest' in
           Exp (left, right), rest''
         | _ -> left, rest)
    and parse_atom = function
      | [] -> failwith "Empty input"
      | Ln :: rest ->
        let expr, rest' = parse_atom rest in
        Ln expr, rest'
      | Sin :: rest ->
        let expr, rest' = parse_atom rest in
        Sin expr, rest'
      | Cos :: rest ->
        let expr, rest' = parse_atom rest in
        Cos expr, rest'
      | Tan :: rest ->
        let expr, rest' = parse_atom rest in
        Tan expr, rest'
      | LDiff :: rest ->
        let expr, rest' = parse_atom rest in
        (match rest' with
         | Wrt :: LVar x :: rest'' -> Diff (expr, x), rest''
         | _ -> failwith "Invalid differentiation syntax")
      | LIntegral :: rest ->
        let expr, rest' = parse_atom rest in
        (match rest' with
         | Wrt :: LVar x :: LCBracket :: Nat l1 :: Comma :: Nat l2 :: RCBracket :: rest''
           -> Integral (expr, x, Some (l1, l2)), rest''
         | Wrt :: LVar x :: rest'' -> Integral (expr, x, None), rest''
         | _ -> failwith "Invalid Integral syntax")
      | LVar x :: rest ->
        (match Hashtbl.find_opt ctx x with
         | Some xpr -> xpr, rest
         | None -> Var x, rest)
      | Let :: LVar x :: Assign :: rest ->
        let expr, rest' = parse_add rest in
        Hashtbl.add ctx x expr;
        expr, rest'
      | Nat n :: rest -> Const n, rest
      | E :: rest -> E, rest
      | LParen :: rest ->
        let expr, rest' = parse_add rest in
        (match rest' with
         | RParen :: rest'' -> expr, rest''
         | _ -> failwith "Missing closing parenthesis")
      | _ -> failwith "Invalid token"
    in
    let expr, rest = parse_add tokens in
    match rest with
    | EOF :: _ -> expr
    | _ -> failwith "Unexpected tokens after expression"
  ;;
end

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * if n mod 2 = 0 then 1 else a
;;

let rec subst x s expr =
  match expr with
  | Var v when x = v -> s
  | Mul (e1, e2) -> Mul (subst x s e1, subst x s e2)
  | Div (e1, e2) -> Div (subst x s e1, subst x s e2)
  | Add (e1, e2) -> Add (subst x s e1, subst x s e2)
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
    match expression with
    | Exp (Const c, Var _) -> Mul (expression, Ln (Const c))
    | Mul (E, Var _) | Mul (Var _, E) -> E
    | Mul (Const _, E) | Mul (E, Const _) | Exp (E, Var _) -> expression
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
      Div (Sub (Mul (e1', e2) |> simplify, Mul (e1, e2') |> simplify), Exp (e2, Const 2.))
      |> simplify
    | Exp (Var x, Const c) -> Mul (Const c, simplify (Exp (Var x, Const (c -. 1.))))
    | Exp (e1, Const c) ->
      Mul
        ( Mul (Const c, Exp (e1, Const (c -. 1.))) |> simplify
        , derivative_engine e1 wrt |> simplify )
      |> simplify
    | Cos (Var x) -> Mul (Const (-1.), Sin (Var x))
    | Sin xpr -> Mul (derivative_engine xpr wrt, Cos xpr)
    | _ -> failwith "Not implemented yet"
  in
  let rec integral_engine (expression : expression) wrt (limits : (float * float) option) =
    let res =
      match simplify expression with
      | Var x when x = wrt ->
        let num = Exp (Var x, Const 2.) |> simplify in
        Div (num, Const 2.) |> simplify
      | Exp (Var v, Const c) ->
        let num = Exp (Var v, Const (c +. 1.)) |> simplify in
        Div (num, Const (c +. 1.)) |> simplify
      | Const c -> Mul (Const c, Var wrt)
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
      | Add (e1, e2) ->
        Add
          ( integral_engine (simplify e1) wrt limits
          , integral_engine (simplify e2) wrt limits )
      | Sub (e1, e2) ->
        Sub
          ( integral_engine (simplify e1) wrt limits
          , integral_engine (simplify e2) wrt limits )
      | _ -> expression
    in
    (* seperate in a seperate function to avoid + c's being appended in recursive calls *)
    match limits with
    | Some (l1, l2) ->
      Sub (subst wrt (Const l2) res |> simplify, subst wrt (Const l1) res |> simplify)
      |> simplify
    | None -> Add (res, Var "c")
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
  | Add (e1, e2) -> Add (simplify e1, simplify e2) |> simplify'
  | Mul (e1, e2) -> Mul (simplify e1, simplify e2) |> simplify'
  | Div (e1, e2) -> Div (simplify e1, simplify e2) |> simplify'
  | Exp (e1, e2) -> Exp (simplify e1, simplify e2) |> simplify'
  | Sin e -> Sin (simplify e) |> simplify'
  | Cos e -> Cos (simplify e) |> simplify'
  | Tan e -> Tan (simplify e) |> simplify'
  | Ln e -> Ln (simplify e) |> simplify'
  | Diff (expression, x) -> derivative_engine (simplify expression) x |> simplify
  | Integral (expression, x, Some (l1, l2)) ->
    integral_engine (simplify expression) x (Some (l1, l2)) |> simplify
  | Integral (expression, x, None) ->
    integral_engine (simplify expression) x None |> simplify
  | _ -> simplify' expr
;;

let p x = x |> Lexer.lex |> Parser.parse |> simplify |> pp
let pl x = x |> Lexer.lex |> Parser.parse |> simplify |> pp_latex

let () =
  let _ = p ("let pi = " ^ string_of_float pi) in
  let _ = p ("let ev = " ^ string_of_float e) in
  ()
;;
