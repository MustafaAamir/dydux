open Dydux.Engine
open Dydux.Expr
open Stdlib

let p x =
  x
  |> Lexer.lex
  |> Parser.parse
  |> Engine.simplify
  |> Engine.post_process_integral integral_flag
;;

let banner =
  "\n\
  \       _           _             \n\
  \      | |         | |            \n\
  \    __| |_   _  __| |_   _ _   _\n\
  \   / _  | | | |/ _  | | | ( \\ / )\n\
  \  ( (_| | |_| ( (_| | |_| |) X ( \n\
  \  \\____|\\__  |\\____|__/_/ \\_) \\_)\n\
  \       (____/                   \n\n\
  \   A mathematical inference engine\n"
;;

let () =
  let _ = p ("let pi = " ^ string_of_float pi) in
  let _ = p ("let ev = " ^ string_of_float e) in
  ()
;;

let toggle_latex = ref false

(*
   let rec input prompt cb =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    cb v;
    input prompt cb
;;

let () =
  LNoise.history_load ~filename:"history.txt" |> ignore;
  LNoise.history_set ~max_length:1000 |> ignore;
  LNoise.set_hints_callback (fun line ->
    if line <> "integrate" && line <> "diff "
    then None
    else Some (" <expression> wrt <variable>", LNoise.Yellow, true));
  LNoise.set_completion_callback (fun line_so_far ln_completions ->
    if line_so_far <> "" && line_so_far.[0] = 'i'
    then "integrate" |> LNoise.add_completion ln_completions
    else if line_so_far <> "" && line_so_far.[0] = 'd'
    then "diff" |> LNoise.add_completion ln_completions);
  (fun from_user ->
    if from_user = "quit" || from_user = "exit"
    then exit 0
    else if from_user = "#toggle_latex"
    then toggle_latex := not !toggle_latex
    else if from_user = "clear"
    then (
      let _ = Sys.command "clear" in
      ())
    else (
      LNoise.history_add from_user |> ignore;
      LNoise.history_save ~filename:"history.txt" |> ignore;
      let result = Expr.p from_user in
      Printf.sprintf "   = %s" result |> print_endline;
      if !toggle_latex = true
      then (
        let result = Expr.pl from_user in
        Printf.sprintf "TeX = %s" result |> print_endline)))
  |> input " λ > "
;;
*)
let () = print_endline banner

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
      let result = p input in
      if !toggle_latex = true
      then (
        result |> P.latex |> print_endline;
        repl ())
      else (
        let result = p input in
        result |> P.print |> print_endline;
        repl ())
    with
    | _ ->
      print_endline "Error";
      repl ())
;;

let () = repl ()
