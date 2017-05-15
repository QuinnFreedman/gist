open OUnit2;;
open Batteries;;
open Printf;;

(*
let _ =
  "mytest">::
  (fun test_ctxt ->
    assert_equal
      ~msg:"int value"
      ~printer:string_of_int
      1
      (Foo.unity 1))
;;

 *)


run_test_tt_main LexerTests.lexer_suite;;
run_test_tt_main ParserLibTests.parser_lib_suite;;
run_test_tt_main ParserTests.parser_suite;;
run_test_tt_main CheckCorrectnessTests.correctness_checker_suite;;

