let version = "          Dydux version 0.0.1\n"
let description = "     A mathematical inference engine\n\n"

let banner =
  "\n\
  \         _           _             \n\
  \        | |         | |            \n\
  \      __| |_   _  __| |_   _ _   _\n\
  \     / _  | | | |/ _  | | | ( \\ / )\n\
  \    ( (_| | |_| ( (_| | |_| |) X ( \n\
  \    \\____|\\__  |\\____|__/_/ \\_) \\_)\n\
  \          (____/                   \n\n"
  ^ version
  ^ description
;;

open Dydux.Engine
open Dydux.Expr
open Dydux.Types
open Stdlib
open ANSITerminal

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

let printer e =
  match !toggle_latex with
  | true -> P.latex e
  | false -> P.print e
;;

let () = ANSITerminal.print_string [ Foreground White; Bold ] banner

let print_cursor p =
  ANSITerminal.printf [ Bold; Foreground Red ] "%s^" (String.make (p + 5) ' ')
;;

let print_row k v =
  let k' = ANSITerminal.sprintf [ Foreground White; Bold ] "%s" k in
  let v' = ANSITerminal.sprintf [ Foreground White ] "%s" (printer v) in
  Printf.printf "%10s ➞ %s\n" k' v'
;;

let rec repl () =
  ANSITerminal.print_string [ Foreground Yellow; Bold; Blink ] " λ > ";
  flush stdout;
  let input = read_line () in
  try
    (match input with
     | "exit" | "quit" -> exit 0
     | "clear" | "cls" ->
       let _ = Sys.command "clear" in
       ()
     | ":toggle_latex" -> toggle_latex := not !toggle_latex
     | ":context" -> Hashtbl.iter (fun k v -> print_row k v) ctx
     | _ ->
       Hashtbl.remove ctx "$";
       let result = p input in
       Hashtbl.add ctx "$" result;
       result |> printer |> print_endline);
    repl ()
  with
  | Invalid_argument _ -> repl ()
  | Parser_error (c, p) ->
    print_cursor p;
    Printf.printf " Lexer: %s\n" c;
    repl ()
  | Lexer_error (c, p) ->
    print_cursor p;
    Printf.printf " Parser: %s\n" c;
    repl ()
  | Failure c ->
    Printf.printf " Error: %s\n" c;
    repl ()
;;

let () = repl ()
