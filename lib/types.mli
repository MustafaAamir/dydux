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

exception Parser_error of string * int
exception Lexer_error of string * int
exception Engine_error of string

type ctxt = (string, expression) Hashtbl.t
