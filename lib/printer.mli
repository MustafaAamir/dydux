module P :
  sig
    val safe_int_to_string : float -> string
    val prec : Types.expression -> int
    val paren : int -> Types.expression -> string
    val print : Types.expression -> string
    val latex_paren : int -> Types.expression -> string
    val latex : Types.expression -> string
    val pp_expression : Format.formatter -> Types.expression -> unit
    val dump_ast : Types.expression -> unit
  end
