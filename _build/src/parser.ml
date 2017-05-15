open Batteries;;
open Printf;;
open Types;;
open Lexer;;
open ParserLib;;

let keywords = ["def"; "defun"];;

let p_ident_exclude_keywords =
  p_any_ident ~keywords:keywords
;;

let g_int = p_int >>> 
  (fun t -> 
  match t with
    | INT (i, p) -> Int (i, p)
    | _ -> failwith "parse error");;

let g_var = p_ident_exclude_keywords>>> 
  (fun t -> 
  match t with
    | IDENT (_, s, p) -> Var (s, p)
    | _ -> failwith "parse error");;

let g_def = p_ident "def" >>> (fun _ -> ());;
let g_defun = p_ident "defun" >>> (fun _ -> ());;

let lparen_pos = lparen >>> position_of_token;;
let rparen_pos = rparen >>> position_of_token;;
let lparen_tick_pos = lparen_tick >>> position_of_token;;
let ident_str = p_ident_exclude_keywords >>> token_to_str;;

let g_expr = fix (fun g_expr ->
  let g_list = 
    lparen_tick *> (many g_expr) <* rparen
  in
  let g_call = 
    lparen_pos <*> ident_str <*> (many g_expr) <*> rparen_pos >>>
    (fun (((lp, name), arg_list), rp) -> 
       let (start_line, start_col, _, _) = lp in
       let (_, _, end_line, end_col) = rp in
       Call (name, arg_list, (start_line, start_col, end_line, end_col)))
  in
  let g_let =
    lparen_pos <* g_def <*> ident_str <*> g_expr <*> g_expr <*> rparen_pos >>>
    (fun (((((start_line, start_col, _, _), name), expr1), expr2), 
      (_, _, end_line, end_col)) -> 
    Let(name, expr1, expr2, (start_line, start_col, end_line, end_col))) 
  in
  let g_fun =
    lparen_pos <* g_defun <*> ident_str <*> 
        (lparen_tick *> (many ident_str) <* rparen)
        <*> g_expr <*> rparen_pos >>>
        (fun (((((start_line, start_col, _, _), name), arg_list), body),
              (_, _, end_line, end_col)) ->
          Defun(name, arg_list, body, (start_line, start_col, end_line, end_col)))
  in
  g_fun <?> g_let <?> g_call <?> g_int <?> g_var) 
;;

let parse_string s =
  s 
    |> lex
    |> parse g_expr
;;
