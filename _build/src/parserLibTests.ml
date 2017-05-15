open OUnit2;;
open Batteries;;
open Printf;;

open Types;;
open ParserLib;;
open Lexer;;

let print_parse_result result = 
  match result with
  | ParseSuccess (output, input_list) ->
    sprintf "ParseSuccess(%s, %s)" (dump output) (dump input_list)
  | ParseFailure (str, pos) ->
    sprintf "ParseFailure(\"%s\", %s)" str (dump pos)

let parser_tests =
  [
    "always pass">::
    (fun _ ->
       assert_equal
         ~printer:dump
         1 1);

    "parse defun" >::
    (fun _ ->
       let defun_token = IDENT(false, "defun", (1, 1, 1, 5)) in
       assert_equal
         ~printer:dump
         (ParseSuccess (defun_token, []))
         (p_ident "defun" [defun_token]));

    "p_ident fail" >::
    (fun _ ->
       let defun_token = IDENT(false, "defun", (1, 1, 1, 5)) in
       assert_equal
         ~printer:dump
         (ParseFailure ("`zzzz`", (1, 1, 1, 5)))
         (p_ident "zzzz" [defun_token]));

    "parse two idents" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = (p_ident "a") <*> (p_ident "b") in
       assert_equal
         ~printer:dump
         (ParseSuccess ((a_token, b_token), []))
         (parser [a_token; b_token]));

    "fail <*>" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = (p_ident "a") <*> (p_ident "zzzz") in
       assert_equal
         ~printer:dump
         (ParseFailure ("`zzzz`", (1, 3, 1, 3)))
         (parser [a_token; b_token]));

    "a *> b" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = (p_ident "a") *> (p_ident "b") in
       assert_equal
         ~printer:dump
         (ParseSuccess (b_token, []))
         (parser [a_token; b_token]));

    "a <* b" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = (p_ident "a") <* (p_ident "b") in
       assert_equal
         ~printer:dump
         (ParseSuccess (a_token, []))
         (parser [a_token; b_token]));

    "(a <* b) >>> (fun ...)" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = ((p_ident "a") <* (p_ident "b")) >>>
         (fun token -> match token with
           | IDENT(_, name, _) -> name
           | _ -> failwith "err")
       in
       assert_equal
         ~printer:dump
         (ParseSuccess ("a", []))
         (parser [a_token; b_token]));

    "(a <* b) >>> (fun ...)" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = ((p_ident "a") <* (p_ident "b")) >>>
         (fun token -> match token with
           | IDENT(_, name, _) -> name
           | _ -> failwith "err")
       in
       assert_equal
         ~printer:dump
         (ParseSuccess ("a", []))
         (parser [a_token; b_token]));

    "a <?> b" >::
    (fun _ ->
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       let parser = (p_ident "a") <?> (p_ident "b") in
       assert_equal
         ~printer:dump
         (ParseSuccess (b_token, []))
         (parser [b_token]));

    "test `any` none" >::
    (fun _ ->
       assert_equal
         ~printer:dump
         (ParseSuccess([], []))
         ((many (p_ident "x")) []));

    "test `many` one" >::
    (fun _ ->
       let a_token  = IDENT(false, "a", (1, 1, 1, 1)) in
       let b_token  = IDENT(false, "b", (1, 3, 1, 3)) in
       assert_equal
         ~printer:dump
         (ParseSuccess([a_token], [b_token]))
         ((many (p_ident "a")) [a_token; b_token]));

    "test `many`" >::
    (fun _ ->
       let x_token  = IDENT(false, "x", (0,0,0,0)) in
       let parser = many (p_ident "x") in
       assert_equal
         ~printer:dump
         (ParseSuccess ([x_token; x_token; x_token], []))
         (parser [x_token; x_token; x_token]));

    "test `many1`" >::
    (fun _ ->
       let x_token  = IDENT(false, "x", (0,0,0,0)) in
       assert_equal
         ~printer:dump
         (ParseSuccess ([x_token; x_token; x_token], []))
         (many1 (p_ident "x") [x_token; x_token; x_token]));

    "test `many1` fail" >::
    (fun _ ->
       let x_token  = IDENT(false, "x", (0,0,0,0)) in
       assert_equal
         ~printer:dump
         (ParseFailure ("`z`", (0,0,0,0)))
         (many1 (p_ident "z") [x_token; x_token; x_token]));

    "test p_int" >::
    (fun _ ->
       let x_token  = INT(199, (0,0,0,0)) in
       assert_equal
         ~printer:dump
         (ParseSuccess (x_token, []))
         (p_int [x_token]));

    "test p_lparen" >::
    (fun _ ->
       let token  = LPAREN(false, (0,0,0,0)) in
       assert_equal
         ~printer:dump
         (ParseSuccess (token, []))
         (p_paren '(' [token]));

    "a <?> b" >::
    (fun _ ->
       let token  = IDENT(false, "z", (1, 3, 1, 3)) in
       let parser = (p_ident "a") <?> (p_ident "b") in
       assert_equal
         ~printer:dump
         (ParseFailure ("`a` or `b`", (1, 3, 1, 3)))
         (parser [token]));

    "test generic ident" >::
    (fun _ ->
       let token  = IDENT(false, "test", (0,0,0,0)) in
       assert_equal
         ~printer:dump
         (ParseSuccess (token, []))
         (p_any_ident [token]));

    "test parse" >::
    (fun _ ->
       let tokens = Lexer.lex "(defun a x)" in
       let test_parser: (token, string list) parser =
         lparen *> 
          (
          ((p_ident "defun") <*> p_any_ident <*> p_any_ident) >>> 
          (fun ((a, b), c) -> [token_to_str a; token_to_str b; token_to_str c])
          ) 
          <* rparen
       in
       assert_equal
         ~printer:dump
         ["defun"; "a"; "x"]
         (parse test_parser tokens));

    "test `fix`" >::
    (fun _ ->
       let a_token = IDENT(false, "a", (0,0,0,0)) in
       let num_token = INT(999999999, (0,0,0,0)) in
       let expr = fix (fun expr ->
           let a = (p_ident "a") *> expr in
           a <?> p_int)
       in
        assert_equal
          ~printer:dump
          (ParseSuccess(num_token, []))
          (expr [a_token; a_token; num_token;]));

    "test '( vs (" >::
    (fun _ ->
       assert_equal
         ~printer:print_parse_result
         (ParseFailure("`'(`", (0,0,0,0)))
         (lparen_tick [LPAREN(false, (0,0,0,0))]));

    "parse_any_ident reject keyword" >::
    (fun _ ->
       assert_equal
         ~printer:print_parse_result
         (ParseFailure("an identifier", (0,0,0,0)))
         (p_any_ident ~keywords:["def";"defun"] [IDENT(false, "defun", (0,0,0,0))]));

    "<?> error presidence" >::
    (fun _ ->
       let p1 = lparen *> (p_ident "def") *> p_any_ident in
       let p2 = p_int in
       let p = p1 <?> p2 in
       assert_equal
         ~printer:print_parse_result
         (ParseFailure("an identifier", (1,6,1,6)))
         ("(def 9)" |> lex |> p));

  ];;

let parser_lib_suite = "parser tests suite" >::: parser_tests;;
