open Batteries;;
open String;;
open Printf;;
open Types;;

let clamp x = 
  if x > 0 then 1
  else if x < 0 then -1
  else 0
;;

let compare_position ((line, col, _, _): position) 
    ((line', col', _, _): position): int =
  let line_dif = line - line' in
  if line_dif != 0 then clamp line_dif else clamp (col - col')
;;

let consume_ident s line_start col_start =
  match s with
  | [] -> None
  | _ ->
      begin
        let (tick, str) =
          match s with
          | '\''::rest -> (true, rest)
          | _ -> (false, s)
        in
        let rec loop s acc =
          match s with
          | [] -> acc, []
          | c::rest ->
              if Char.is_letter c || Char.is_digit c ||
                 Char.is_symbol c then
                (loop rest (acc@[c]))
              else
              acc, s
        in
        let (token_string, rest) = loop str [] in
        match token_string with
        | [] -> None
        | _ -> Some (IDENT(tick, String.implode token_string,
                           (line_start, col_start,
                            line_start,
                            col_start + (List.length token_string) - 1)), rest)
      end
;;


let consume_int s line_start col_start =
  match s with
  | [] -> None
  | _ ->
      begin
        let (negative, str) =
          match s with
          | '-'::rest -> (true, rest)
          | _ -> (false, s)
        in
        let rec loop s acc =
          match s with
          | [] -> acc, []
          | c::rest ->
              if Char.is_digit c then
                (loop rest (acc@[c]))
              else
              acc, s
        in
        let (token_string, rest) = loop str [] in
        let token_string' =
          if negative then
            "-" ^ String.implode token_string
          else
          String.implode token_string
        in
        match token_string with
        | [] -> None
        | _ -> Some (
            INT (int_of_string token_string',
                 (line_start, col_start,
                  line_start,
                  col_start + (String.length token_string') - 1)), rest)
      end
;;

let consume_paren s line_start col_start =
  match s with
  | '('::rest -> Some (LPAREN(false,
                              (line_start, col_start,
                               line_start, col_start)), rest)
  | ')'::rest -> Some (RPAREN((line_start, col_start,
                               line_start, col_start)), rest)
  | '\''::'('::rest -> Some (LPAREN(true,
                                    (line_start, col_start,
                                     line_start, col_start + 1)), rest)
  | _ -> None

;;

let rec consume_whitespace s line col =
  match s with
  | [] -> s, line, col
  | c::rest ->
      if c = '\n' then
        consume_whitespace rest (line + 1) 1
      else if c = '\r' then
        consume_whitespace rest line col
      else if Char.is_whitespace c then
        consume_whitespace rest line (col + 1)
      else
        s, line, col
;;


let ( <=> ) (a: 'a option) (b: 'a option): 'a option =
  match a with
  | Some _ -> a
  | None -> b
;;


let lex (s: string): token list =
  let rec loop input line_start col_start token_list =
    let s, line, col = consume_whitespace input line_start col_start in
    match s with
    | [] -> token_list
    | _ ->
        let result = (consume_paren s line col) <=>
                     (consume_int s line col)   <=>
                     (consume_ident s line col) in
        match result with
        | Some (token, rest) ->
            let line, col =
              match token with
              | LPAREN (_, (_, _, l, c)) -> (l, c)
              | RPAREN ((_, _, l, c)) -> (l, c)
              | IDENT (_, _, (_, _, l, c)) -> (l, c)
              | INT (_, (_, _, l, c)) -> (l, c)
            in
            loop rest line (col + 1) (token_list@[token])
        | None -> failwith
                    (sprintf
                       "Error @ (%d, %d): unexpected character `%c`"
                       line col (List.first s))

  in
  loop (explode s) 1 1 []
;;
