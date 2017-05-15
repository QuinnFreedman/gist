open Batteries;;
open OUnit2;;
open Types;;
open Lexer;;
open Parser;;
open ParserLib;;

let parser_tests =
  [
    "always pass" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         1 1);

    "parse int" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         (Int(-9, (1,1,1,2)))
         (parse g_int (lex "-9")));

    "parse var" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         (Var("x", (1,1,1,1)))
         (parse g_var (lex "x")));

    "parse def" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         ()
         (parse g_def (lex "def")));

    "parse defun" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         ()
         (parse g_defun (lex "defun")));
    
    "parse function call" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         (Call ("my-func", [Int(0, (1,10,1,10)); Int(1, (1,12,1,12))], (1,1,1,13)))
         (parse g_expr (lex "(my-func 0 1)")));
   
    "parse def" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         (Let ("x", Int(0, (1,8,1,8)), Var("x", (1,10,1,10)), (1,1,1,11)))
         (parse g_expr (lex "(def x 0 x)")));

    "parse defun" >::
    (fun _ ->
       assert_equal
         ~printer:string_of_expr
         (Defun ("foo", ["x"], Var("x", (1,17,1,17)), (1,1,1,18)))
         ("(defun foo '(x) x)" |> lex |> (parse g_expr)));

    "parse fail defun" >::
    (fun _ ->
       assert_raises
         (Failure("Error @ (1:12): expected `'(`"))
         (fun () -> ("(defun foo (x) x)" |> lex |> (parse g_expr))));

    "parse fail keyword" >::
    (fun _ ->
       assert_raises
         (Failure("Error @ (1:17): expected `(` or an integer or an identifier"))
         (fun () -> ("(defun foo '(a) def)" |> lex |> (parse g_expr))));

  ];;

let parser_suite = "parser test suite" >::: parser_tests;;
