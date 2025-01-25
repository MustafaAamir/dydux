type expression =
  | Var of string
  | Const of float
  | Add of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Exp of expression * expression
  | Sin of expression
  | Cos of expression
  | Tan of expression
  | Diff of expression * string
  | Let of string * expression

type ctxt = (string, expression) Hashtbl.t

let ctx = Hashtbl.create 0

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
    | LDiff
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

  let nat st i =
    let j = ref (i + 1) in
    while !j < st.len && is_num st.input.[!j] do
      incr j
    done;
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
           | _ -> LVar var :: advance st (!j - st.pos))
        | '0' .. '9' ->
          let var, j = nat st st.pos in
          Nat (float_of_string var) :: advance st (!j - st.pos)
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
      | LVar x :: rest ->
        (match Hashtbl.find_opt ctx x with
         | Some xpr -> xpr, rest
         | None -> Var x, rest)
      | Let :: LVar x :: Assign :: rest ->
        let expr, rest' = parse_atom rest in
        Hashtbl.add ctx x expr;
        expr, rest'
      | Nat n :: rest -> Const n, rest
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

let rec simplify expr =
  let rec derivative_engine expression wrt =
    match expression with
    | Const _ -> Const 0.0
    | Var _ -> Const 1.
    | Mul (Const c, Var _) -> Const c
    | Mul (Var _, Const c) -> Const c
    | Mul (e1, e2) ->
      print_endline "here";
      let e1' = derivative_engine e1 wrt in
      let e2' = derivative_engine e2 wrt in
      Add (Mul (e1', e2), Mul (e1, e2'))
    | Exp (Var x, Const c) -> Mul (Const c, Exp (Var x, Const (c -. 1.)))
    | Cos (Var x) -> Mul (Const (-1.), Sin (Var x))
    | Sin xpr -> Mul (derivative_engine xpr wrt, Cos xpr)
    | _ -> failwith "Not implemented yet"
  in let simplify' = function
    | Add (Const m, Const n) -> Const (m +. n)
    | Mul (Const m, Const n) -> Const (m *. n)
    | Mul (Var m, Var n) when m = n -> Exp(Var m, Const 2.)
    | Add (Var m, Var n) when m = n -> Mul(Const 2., Var m)
    | Add (Const 0., x) | Add (x, Const 0.) -> x
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
    | _ -> expr
  in
  match expr with
  | Add (e1, e2) -> Add (simplify e1, simplify e2) |> simplify'
  | Mul (e1, e2) -> Mul (simplify e1, simplify e2) |> simplify'
  | Div (e1, e2) -> Div (simplify e1, simplify e2) |> simplify'
  | Exp (e1, e2) -> Exp (simplify e1, simplify e2) |> simplify'
  | Sin e -> Sin (simplify e) |> simplify'
  | Cos e -> Sin (simplify e) |> simplify'
  | Tan e -> Sin (simplify e) |> simplify'
  | Diff (expression, x) -> derivative_engine (simplify expression) (Var x) |> simplify'
  | _ -> simplify' expr
;;

let safe_int_to_string x =
  let diff = abs_float (x -. float_of_int (int_of_float x)) in
  if diff < 1e-9 then string_of_int (int_of_float x) else string_of_float x

let rec pp = function
  | Const x -> safe_int_to_string x
  | Var x -> x
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (pp e1) (pp e2)
  | Mul(Const c, Var x) | Mul(Var x, Const c) -> Printf.sprintf "(%s%s)" (pp @@ Const c) (pp @@ Var x)
  | Mul(Const c, e1) -> Printf.sprintf "(%s(%s))" (pp @@ Const c) (pp e1);
  | Mul (e1, e2) -> Printf.sprintf "(%s * %s)" (pp e1) (pp e2)
  | Div (e1, e2) -> Printf.sprintf "(%s / %s)" (pp e1) (pp e2)
  | Exp (e1, e2) -> Printf.sprintf "(%s ^ %s)" (pp e1) (pp e2)
  | Sin e1 -> Printf.sprintf "sin(%s)" (pp e1)
  | Cos e1 -> Printf.sprintf "cos(%s)" (pp e1)
  | Tan e1 -> Printf.sprintf "tan(%s)" (pp e1)
  | Diff (expression, x) -> Printf.sprintf "%s .wrt %s" (pp expression) x
  | Let (var, expr) -> Printf.sprintf "%s = %s" var (pp expr)
;;

let p x = x |> Lexer.lex |> Parser.parse |> simplify |> pp
