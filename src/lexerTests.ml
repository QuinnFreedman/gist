open Batteries;;
open OUnit2;;
open String;;
open Types;;
open Lexer;;

let always_pass =
  "this test should always pass" >::
  (fun _ ->
     assert_equal 1 1)
;;

let lex_lparen =
  "lex lparen" >::
  (fun _ ->
     let tokens = lex "(" in
     assert_equal [LPAREN(false, (1, 1, 1, 1))] tokens)
;;

let lex_rparen =
  "lex rparen" >::
  (fun _ ->
     let tokens = lex ")" in
     assert_equal [RPAREN((1, 1, 1, 1))] tokens)
;;

let lex_lparen_ident =
  "lex lparen" >::
  (fun _ ->
     let tokens = lex "(asdf" in
     assert_equal [LPAREN(false, (1, 1, 1, 1));
                   IDENT(false, "asdf", (1,2,1,5))]
       tokens)
;;

let consume_ident_test =
  "consume asdf ghjk" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (IDENT(false, "asdf", (1,1,1,4)), String.explode " ghjk"))
       (consume_ident (String.explode "asdf ghjk") 1 1))
;;

let consume_ident_eof =
  "consume last ident" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (IDENT(false, "asdf", (1,1,1,4)), []))
       (consume_ident (String.explode "asdf") 1 1))
;;

let consume_int_test =
  "consume 123asdf" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (INT(123, (1,1,1,3)), String.explode "asdf"))
       (consume_int (String.explode "123asdf") 1 1))
;;

let consume_lparen_test =
  "consume `(`" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (LPAREN(false, (1,1,1,1)), []))
       (consume_paren (String.explode "(") 1 1))
;;

let consume_rparen_test =
  "consume `)`" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (RPAREN((1,1,1,1)), []))
       (consume_paren (String.explode ")") 1 1))
;;

let consume_lparen_tick =
  "consume `'(`" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (RPAREN((1,1,1,1)), []))
       (consume_paren (String.explode "'(") 1 1))
;;

let test_consume_whitespace =
  "consume whitespace" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (String.explode "test", 1, 4)
       (consume_whitespace (String.explode "   test") 1 1))
;;

let test_consume_whitespace_newline =
  "consume whitespace newline" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (String.explode "test", 2, 2)
       (consume_whitespace (String.explode "  \n test") 1 1))
;;

let test_consume_negative_number =
  "consume -12" >::
  (fun _ ->
     assert_equal
       ~printer:dump
       (Some (INT(-12, (1,1,1,3)), []))
       (consume_int (String.explode "-12") 1 1))
;;

let test_lex_list =
  "lex (test '(-3 a))" >::
  (fun _ ->
     let tokens = lex "(test '(-3 a))" in
     assert_equal
       ~printer:dump
       [LPAREN(false, (1, 1, 1, 1));
        IDENT(false, "test", (1, 2, 1, 5));
        LPAREN(true, (1, 7, 1, 8));
        INT(-3, (1, 9, 1, 10));
        IDENT(false, "a", (1, 12, 1, 12));
        RPAREN((1, 13, 1, 13));
        RPAREN((1, 14, 1, 14));
       ] tokens)
;;


let lexer_tests =
  [
    "compare pos" >::
    (fun _ ->
       assert_equal
         (-1)
         (compare_position (1,1,1,1) (2,2,2,2))
    );
    
    "compare pos same line" >::
    (fun _ ->
       assert_equal
         ~printer:string_of_int
         (1)
         (compare_position (2,2,2,2) (2,1,2,1))
    );

    "compare pos differnt" >::
    (fun _ ->
       assert_equal
         ~printer:string_of_int
         (1)
         (compare_position (2,5,2,5) (2,1,2,1))
    );


    always_pass;
    consume_ident_test;
    consume_ident_eof;
    consume_int_test;
    test_consume_negative_number;
    consume_lparen_test;
    consume_rparen_test;
    lex_lparen;
    lex_rparen;
    lex_lparen_ident;
    test_consume_whitespace;
    test_consume_whitespace_newline;
    test_lex_list;

  ];;

let lexer_suite = "lexer tests suite" >::: lexer_tests;;
