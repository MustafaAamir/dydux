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

type ctxt = (string, expression) Hashtbl.t
