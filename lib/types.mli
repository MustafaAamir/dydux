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
  | Sec of expression
  | Cosec of expression
  | Cot of expression
  | Arccos of expression
  | Arcsin of expression
  | Arctan of expression
  | Diff of expression * string
  | E
  | Ln of expression
  | Log of expression * expression
  | Let of string * string list option * expression
  | Integral of expression * string * (expression * expression) option
[@@deriving show { with_path = false }, eq]

exception Parser_error of string * int
exception Lexer_error of string * int
exception Engine_error of string

type ctxt = (string, expression) Hashtbl.t
