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
        (fst limits |> print)
        (snd limits |> print)
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
      Printf.sprintf "\\int_{%s}^{%s} %s\\, d%s" (print a) (print b) (latex expression) x
    | Integral (expression, x, None) ->
      Printf.sprintf "\\int %s\\, d%s" (latex expression) x
    | Let (var, expr) -> Printf.sprintf "%s = %s" var (latex expr)
  ;;

  let rec pp_expression fmt = function
  | Var s -> Format.fprintf fmt "Var(%S)" s
  | Const f -> Format.fprintf fmt "Const(%f)" f
  | Add (e1, e2) -> 
      Format.fprintf fmt "Add(@[<hov>%a,@ %a@])" pp_expression e1 pp_expression e2
  | Sub (e1, e2) -> 
      Format.fprintf fmt "Sub(@[<hov>%a,@ %a@])" pp_expression e1 pp_expression e2
  | Mul (e1, e2) -> 
      Format.fprintf fmt "Mul(@[<hov>%a,@ %a@])" pp_expression e1 pp_expression e2
  | Div (e1, e2) -> 
      Format.fprintf fmt "Div(@[<hov>%a,@ %a@])" pp_expression e1 pp_expression e2
  | Exp (e1, e2) -> 
      Format.fprintf fmt "Exp(@[<hov>%a,@ %a@])" pp_expression e1 pp_expression e2
  | Sin e -> Format.fprintf fmt "Sin(@[<hov>%a@])" pp_expression e
  | Cos e -> Format.fprintf fmt "Cos(@[<hov>%a@])" pp_expression e
  | Tan e -> Format.fprintf fmt "Tan(@[<hov>%a@])" pp_expression e
  | Diff (e, s) -> Format.fprintf fmt "Diff(@[<hov>%a,@ %S@])" pp_expression e s
  | E -> Format.fprintf fmt "E"
  | Ln e -> Format.fprintf fmt "Ln(@[<hov>%a@])" pp_expression e
  | Let (s, e) -> Format.fprintf fmt "Let(@[<hov>%S,@ %a@])" s pp_expression e
  | Integral (e, s, None) -> 
      Format.fprintf fmt "Integral(@[<hov>%a,@ %S,@ None@])" pp_expression e s
  | Integral (e, s, Some (e1, e2)) -> 
      Format.fprintf fmt "Integral(@[<hov>%a,@ %S,@ Some(@[<hov>%a,@ %a@])@])" 
        pp_expression e s pp_expression e1 pp_expression e2

let dump_ast expression =
  Format.printf "@[<v>%a@]@." pp_expression expression
end


