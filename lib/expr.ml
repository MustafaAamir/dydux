let ctx = Hashtbl.create 0

module P = struct
  open Types

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

  and paren parent_prec expr =
    let self_prec = prec expr in
    let s = print expr in
    if self_prec < parent_prec then "(" ^ s ^ ")" else s

  and print = function
    | E -> "e"
    | Const x -> safe_int_to_string x
    | Var x -> x
    | Add (e1, e2) -> Printf.sprintf "%s + %s" (paren 1 e1) (paren 1 e2)
    | Sub (e1, e2) -> Printf.sprintf "%s - %s" (paren 1 e1) (paren 2 e2)
    | Mul (Const -1., z) | Mul (z, Const -1.) -> Printf.sprintf "-%s" (print @@ z)
    | Mul (Const c, Var x) -> Printf.sprintf "%s%s" (print @@ Const c) (print @@ Var x)
    | Mul (Const c, e1) -> Printf.sprintf "%s(%s)" (print @@ Const c) (print e1)
    | Mul (e1, e2) -> Printf.sprintf "%s * %s" (paren 2 e1) (paren 2 e2)
    | Div (e1, e2) -> Printf.sprintf "%s / %s" (paren 2 e1) (paren 3 e2)
    | Exp (e1, e2) -> Printf.sprintf "%s ^ %s" (paren 3 e1) (paren 4 e2)
    | Sin e1 -> Printf.sprintf "sin(%s)" (print e1)
    | Cos e1 -> Printf.sprintf "cos(%s)" (print e1)
    | Tan e1 -> Printf.sprintf "tan(%s)" (print e1)
    | Ln e1 -> Printf.sprintf "ln(%s)" (print e1)
    | Diff (expression, x) -> Printf.sprintf "∂%s .wrt %s" (print expression) x
    | Integral (expression, x, Some limits) ->
      Printf.sprintf
        "∫%s .wrt %s{%s to %s}"
        (print expression)
        x
        (fst limits |> safe_int_to_string)
        (snd limits |> safe_int_to_string)
    | Integral (expression, x, None) -> Printf.sprintf "∫%s .wrt %s" (print expression) x
    | Let (var, expr) -> Printf.sprintf "%s = %s" var (print expr)
  ;;

  (*abstract this away*)
  let rec latex_paren parent_prec expr =
    let self_prec = prec expr in
    let s = latex expr in
    if self_prec < parent_prec then "\\left(" ^ s ^ "\\right)" else s

  and latex = function
    | E -> "e"
    | Const x -> safe_int_to_string x
    | Var x -> x
    | Add (e1, e2) -> Printf.sprintf "%s + %s" (latex_paren 1 e1) (latex_paren 1 e2)
    | Sub (e1, e2) -> Printf.sprintf "%s - %s" (latex_paren 1 e1) (latex_paren 2 e2)
    | Mul (Const c, Var x) -> Printf.sprintf "%s%s" (latex (Const c)) (latex (Var x))
    | Mul (Const c, e) -> Printf.sprintf "%s\\left(%s\\right)" (latex (Const c)) (latex e)
    | Mul (e1, e2) -> Printf.sprintf "%s \\cdot %s" (latex_paren 2 e1) (latex_paren 2 e2)
    | Div (e1, e2) -> Printf.sprintf "\\frac{%s}{%s}" (latex e1) (latex e2)
    | Exp (e1, e2) -> Printf.sprintf "%s^{%s}" (latex_paren 3 e1) (latex_paren 4 e2)
    | Sin e -> Printf.sprintf "\\sin\\left(%s\\right)" (latex e)
    | Cos e -> Printf.sprintf "\\cos\\left(%s\\right)" (latex e)
    | Tan e -> Printf.sprintf "\\tan\\left(%s\\right)" (latex e)
    | Ln e -> Printf.sprintf "\\ln\\left(%s\\right)" (latex e)
    | Diff (expression, x) ->
      Printf.sprintf
        "\\frac{\\partial}{\\partial %s} \\left(%s\\right)"
        x
        (latex expression)
    | Integral (expression, x, Some (a, b)) ->
      Printf.sprintf
        "\\int_{%s}^{%s} %s\\, d%s"
        (safe_int_to_string a)
        (safe_int_to_string b)
        (latex expression)
        x
    | Integral (expression, x, None) ->
      Printf.sprintf "\\int %s\\, d%s" (latex expression) x
    | Let (var, expr) -> Printf.sprintf "%s = %s" var (latex expr)
  ;;
end

module Lexer = struct
  open Types

  type token =
    | LParen of int 
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
    | 'A' .. 'Z' | 'a' .. 'z' | '$' -> true
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

  (* fix unary negative and positive sign*)
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
        | '(' -> LParen (st.pos) :: advance st 1
        | ')' -> RParen :: advance st 1
        | ']' -> RBracket :: advance st 1
        | '[' -> LBracket :: advance st 1
        | '}' -> RCBracket :: advance st 1
        | '{' -> LCBracket :: advance st 1
        | ',' -> Comma :: advance st 1
        | '=' -> Assign :: advance st 1
        | x when Array.mem x symbol_arr -> Symbol x :: advance st 1
        | 'A' .. 'Z' | 'a' .. 'z' | '\'' | '$' ->
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
        | _ ->
          let msg =
            Printf.sprintf
              "Unexpected character '%c' at position %d"
              s.[i]
              st.pos
          in
          raise (Lexer_error (msg, st.pos))
      in
      aux 0
    | _ -> [ EOF ]
  ;;
end

module Parser = struct
  open Lexer
  open Types

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
      | LParen c :: rest ->
        let expr, rest' = parse_add rest in
        (match rest' with
         | RParen :: rest'' -> expr, rest''
         | _ -> raise (Parser_error ("Missing closing parenthesis ')' for this pair", c)))
      | _ -> failwith "Invalid token"
    in
    let expr, rest = parse_add tokens in
    match rest with
    | EOF :: _ -> expr
    | _ -> failwith "Unexpected tokens after expression"
  ;;
end
