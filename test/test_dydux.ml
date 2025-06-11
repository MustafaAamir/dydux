open Core_bench
open Dydux.Engine
open Dydux.Expr
open Dydux.Printer
open Dydux.Types [@@warning "-33"]
open OUnit2 [@@warning "-33"]
open Random

let _p x =
  x
  |> Lexer.lex
  |> Parser.parse
  |> Engine.simplify
  |> Engine.post_process_integral integral_flag
;;



module ParserTest = struct
  let process x = x |> Lexer.lex |> Parser.parse |> Engine.simplify
  let engine_process x = x |> Engine.simplify

  let ptest got exp =
    let got' = process got in
    assert_equal ~msg:(P.print got') got' exp (*change later*)
  ;;

  let mutate_rev (x : string) =
    let len = String.length x in
    let res = String.init len (fun n -> String.get x (len - n - 1)) in
    print_endline res;
    res
  ;;

  (* correctly handles x <op> x cases*)
  let dmas_parser (x : string) =
    let ops = [ " + "; " - "; " / "; " * " ] in
    List.map (fun op -> x ^ op ^ x) ops
  ;;

  let dmas_engine (x : string) =
    let expression = process x in
    List.map
      (fun x -> engine_process x)
      [ Add (expression, expression)
      ; Sub (expression, expression)
      ; Div (expression, expression)
      ; Mul (expression, expression)
      ]
  ;;

  let tests =
    "ParserTest"
    >::: [ ("addition" >:: fun _ -> ptest "1 + 1" (Const 2.))
         ; ("dmas_parser"
            >:: fun _ ->
            let expressions = dmas_parser "sin(1 + 1)" in
            let output = dmas_engine "sin(1 + 1)" in
            List.iter (fun x -> ptest (fst x) (snd x)) (List.combine expressions output))
         ]
  ;;
end

let tests = "Main" >::: [ ParserTest.tests ]
let () = run_test_tt_main tests

(*
   module Bench = struct
  let bench =
    Command_unix.run
      (Bench.make_command
         [ Bench.Test.create ~name:"integrate (x) wrt x" (fun () ->
             ignore
               (let x = _p "integrate (x) wrt x" in
                P.print x))
         ])
  ;;
end
*)
