let ctx = Hashtbl.create 0

module Lexer = struct
  open Types

  type token =
    | Let
    | Symbol of char * int
    | LVar of string
    | Sin
    | Cos
    | Tan
    | Ln
    | E
    | LDiff
    | LIntegral
    | Wrt
    | Nat of float
    | EOF
  [@@deriving show { with_path = false }]

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
    | 'A' .. 'Z' | 'a' .. 'z' | '$' | '\'' -> true
    | _ -> false
  ;;

  let is_alphanum c =
    match c with
    | '0' .. '9' -> true
    | c when is_alpha c -> true
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
      [| '^'
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
       ; '('
       ; ')'
       ; ']'
       ; '['
       ; '}'
       ; '{'
       ; ','
       ; '='
      |]
    in
    let keywords =
      [ "sin", Sin
      ; "cos", Cos
      ; "tan", Tan
      ; "diff", LDiff
      ; "wrt", Wrt
      ; "let", Let
      ; "integrate", LIntegral
      ; "e", E
      ; "ln", Ln
      ]
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
        | ' ' | '\t' | '\n' | '\r' -> advance st 1
        | '0' .. '9' ->
          let var, j = nat st st.pos in
          Nat (float_of_string var) :: advance st (!j - st.pos)
        | x when Array.mem x symbol_arr -> Symbol (x, st.pos) :: advance st 1
        | 'A' .. 'Z' | 'a' .. 'z' | '\'' | '$' ->
          let var, j = id st st.pos in
          (match List.assoc_opt var keywords with
           | Some v -> v :: advance st (!j - st.pos)
           | None -> LVar var :: advance st (!j - st.pos))
        | _ ->
          raise
            (Lexer_error
               (Printf.sprintf "Unexpected character " ^ String.make 1 s.[i], st.pos))
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
         | Symbol ('+', _) :: rest' ->
           let right, rest'' = parse_add rest' in
           Add (left, right), rest''
         | Symbol ('-', _) :: rest' ->
           let right, rest'' = parse_add rest' in
           Sub (left, right), rest''
         | _ -> left, rest)
    and parse_mul = function
      | [] -> failwith "Empty input"
      | tokens ->
        let left, rest = parse_exp tokens in
        (match rest with
         | Symbol ('*', _) :: rest' ->
           let right, rest'' = parse_mul rest' in
           Mul (left, right), rest''
         | Symbol ('/', _) :: rest' ->
           let right, rest'' = parse_mul rest' in
           Div (left, right), rest''
         | _ -> left, rest)
    and parse_exp = function
      | [] -> failwith "Empty input"
      | tokens ->
        let left, rest = parse_unary tokens in
        (match rest with
         | Symbol ('^', _) :: rest' ->
           let right, rest'' = parse_exp rest' in
           Exp (left, right), rest''
         | _ -> left, rest)
    and parse_unary = function
      | Symbol ('-', _) :: rest' ->
        let operand, rest'' = parse_unary rest' in
        Mul (Const (-1.), operand), rest''
      | Symbol ('+', _) :: rest' -> parse_unary rest'
      | tokens -> parse_atom tokens
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
        let expr, xs = parse_atom rest in
        (match xs with
         | Wrt :: LVar x :: Symbol ('{', pos) :: rest'' ->
           let e1, rest''' = parse_add rest'' in
           (match rest''' with
            | Symbol (',', _) :: rest1 ->
              let e2, rest1' = parse_add rest1 in
              (match rest1' with
               | Symbol ('}', _) :: rest1'' -> Integral (expr, x, Some (e1, e2)), rest1''
               | _ -> failwith "Expected '}' at the end of integration")
            | _ ->
              failwith
                "Expected ',' after first expression in definite integration syntax")
         | Wrt :: LVar x :: rest'' -> Integral (expr, x, None), rest''
         | _ -> failwith "Invalid Integral syntax")
      | LVar x :: rest ->
        (match Hashtbl.find_opt ctx x with
         | Some xpr -> xpr, rest
         | None -> Var x, rest)
      | Let :: LVar x :: Symbol ('=', _) :: rest ->
        let expr, rest' = parse_add rest in
        Let (x, expr), rest'
      | Nat n :: rest -> Const n, rest
      | E :: rest -> E, rest
      | Symbol ('(', pos) :: rest ->
        let expr, rest' = parse_add rest in
        (match rest' with
         | Symbol (')', _) :: rest'' -> expr, rest''
         | _ ->
           raise (Parser_error ("Missing closing parenthesis ')' for this pair", pos)))
      | (_ as c) :: _ -> failwith ("Invalid token: " ^ show_token c)
    in
    let expr, rest = parse_add tokens in
    match rest with
    | EOF :: _ -> expr
    | _ -> failwith "Unexpected tokens after expression"
  ;;
end
