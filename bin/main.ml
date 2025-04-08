open Matheatrics

let toggle_latex = ref false
let rec repl () =
  print_string "> ";
  flush stdout;
  let input = read_line () in
  if input = "exit"
  then ()
  else if input = "#toggle_latex"
  then (
    toggle_latex := not !toggle_latex;
    repl ())
  else if input = "clear"
  then (
    let _ = Sys.command "clear" in
    repl ())
  else (
    try
      let result = Expr.p input in
      print_endline result;
      if !toggle_latex = true
      then (
        let result = Expr.pl input in
        print_endline result;
        repl ())
      else repl ()
    with
    | _ ->
      print_endline "Error";
      repl ())
;;

let () = repl ()
