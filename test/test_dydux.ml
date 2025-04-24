open Core_bench
open Dydux.Engine
open Dydux.Expr
open Dydux.Printer
(*open Dydux.Types*)

let _p x =
  x
  |> Lexer.lex
  |> Parser.parse
  |> Engine.simplify
  |> Engine.post_process_integral integral_flag
;;

module EngineTest = struct
  let () =
    Command_unix.run
      (Bench.make_command
         [ Bench.Test.create ~name:"integrate (x) wrt x" (fun () ->
             ignore
               (let x = _p "integrate (x) wrt x" in
                P.print x))
         ])
  ;;
end

let () = !integral_flag |> string_of_bool |> print_endline
