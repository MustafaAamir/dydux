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

open Dydux.Engine
open Dydux.Expr
open Dydux.Types
open Stdlib

let p x =
  x
  |> Lexer.lex
  |> Parser.parse
  |> Engine.simplify
  |> Engine.post_process_integral integral_flag
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
  print_string " λ > ";
  flush stdout;
  let input = read_line () in
  try
    (match input with
     | "exit" | "quit" -> ()
     | "clear" | "cls" ->
       let _ = Sys.command "clear" in
       ()
     | "#toggle_latex" -> toggle_latex := not !toggle_latex
     | _ ->
       let result = p input in
       let () = Hashtbl.add ctx "$" result in
       (match !toggle_latex with
        | true -> result |> P.latex |> print_endline
        | false -> result |> P.print |> print_endline));
    repl ()
  with
  | Parser_error (c, p) -> Printf.printf "%s^\n"(String.make (p + 5) ' ') ;Printf.printf "  %s\n" c; repl ()
  | Lexer_error (c, p) -> Printf.printf "%s^\n"(String.make (p + 5) ' ') ;Printf.printf "  %s\n" c; repl ()
  | c -> Printf.printf "%s\n" (Printexc.to_string c);repl ()
;;

let () = repl ()
