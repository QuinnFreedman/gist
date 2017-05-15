open Batteries;;
open Lexer;;
open Printf;;
open Types;;

type ('input, 'output) parse_result =
  | ParseSuccess of 'output * 'input list
  | ParseFailure of string * position


type ('input,'output) parser =
  'input list -> ('input,'output) parse_result
;;


let p_int (input_list: token list): (token, token) parse_result =
     match input_list with
     | [] -> ParseFailure ("an integer", (-1, -1, -1, -1))
     | t::rest ->
         begin
           match t with
           | INT(value, p) ->
               ParseSuccess (t, rest)
           | _ -> ParseFailure ("an integer", position_of_token t)
         end
;;

let tick s = 
  "`" ^ s ^ "`"
;;

let p_ident s =
  (fun input_list ->
     match input_list with
     | [] -> ParseFailure (tick s, (-1, -1, -1, -1))
     | t::rest ->
         begin
           match t with
           | IDENT(_, ident, p) ->
               if ident = s then
                 ParseSuccess (t, rest)
               else
               ParseFailure (tick s, p)
           | _ -> ParseFailure (s, position_of_token t)
         end)
;;



let p_any_ident ?(keywords=[]) input_list =
     match input_list with
     | [] -> ParseFailure ("an identifier", (-1, -1, -1, -1))
     | t::rest ->
         begin
           match t with
           | IDENT(_, ident, p) ->
             if List.mem ident keywords then
               ParseFailure ("an identifier", position_of_token t)
             else
                 ParseSuccess (t, rest)
           | _ -> ParseFailure ("an identifier", position_of_token t)
         end
;;

let p_paren ?(with_tick=false) c =
  let message = tick ((if with_tick then "'" else "") ^ (String.of_char c)) in
  (fun input_list ->
     match input_list with
     | [] -> ParseFailure (message, (-1, -1, -1, -1))
     | t::rest ->
       if c = '(' then
           match t with
           | LPAREN(b, p) ->
             if with_tick = b then
               ParseSuccess (t, rest)
             else
               ParseFailure (message, position_of_token t)
           | _ -> ParseFailure (message, position_of_token t)
       else if c = ')' then
           match t with
           | RPAREN(p) ->
               ParseSuccess (t, rest)
           | _ -> ParseFailure (message, position_of_token t)
       else 
         failwith "argument to p_paren must be '(' or ')'")
;;

let lparen = p_paren '(';;
let rparen = p_paren ')';;
let lparen_tick = p_paren ~with_tick:true '(';;

let ( <*> ) p1 p2 =
  (fun input_list ->
     match p1 input_list with
     | ParseFailure (s, p) -> ParseFailure (s, p)
     | ParseSuccess (x1, rest) ->
         begin
           match p2 rest with
           | ParseFailure (s, p) -> ParseFailure (s, p)
           | ParseSuccess (x2, rest2) ->
               ParseSuccess ((x1, x2), rest2)
         end)

let ( *> ) p1 p2 =
  (fun input_list ->
     match p1 input_list with
     | ParseFailure (s, p) -> ParseFailure (s, p)
     | ParseSuccess (_, rest) ->
         begin
           match p2 rest with
           | ParseFailure (s, p) -> ParseFailure (s, p)
           | ParseSuccess (x, rest2) ->
               ParseSuccess (x, rest2)
         end)

let ( <* ) p1 p2 =
  (fun input_list ->
     match p1 input_list with
     | ParseFailure (s, p) -> ParseFailure (s, p)
     | ParseSuccess (x, rest) ->
         begin
           match p2 rest with
           | ParseFailure (s, p) -> ParseFailure (s, p)
           | ParseSuccess (_, rest2) ->
               ParseSuccess (x, rest2)
         end)

let ( >>> ) (parser: ('input,'output) parser)
    (f: 'output -> 'output2): ('input, 'output2) parser =
  (fun input_list ->
     match parser input_list with
     | ParseFailure (s, p) -> ParseFailure (s, p)
     | ParseSuccess (x, rest) -> ParseSuccess (f x, rest))
;;

let ( <?> ) p1 p2 =
  (fun input_list ->
     match p1 input_list with
     | ParseSuccess (x, rest) ->
         ParseSuccess (x, rest)
     | ParseFailure (expected1, p1) ->
         begin
           match p2 input_list with
           | ParseFailure (expected2, p2) -> 
             begin
               match compare_position p1 p2 with
               | 0 -> 
                 if expected1 = expected2 then
                   ParseFailure (expected1, p1)
                 else
                   ParseFailure (expected1 ^ " or " ^ expected2, p1)
               | -1 -> ParseFailure (expected2, p2)
               | 1 -> ParseFailure (expected1, p1)
               | x -> failwith (sprintf "Error: compare_position returnd %d" x)
             end
           | ParseSuccess (x, rest) ->
               ParseSuccess (x, rest)
         end)
;;

let many (parser: ('a, 'b) parser): ('a, 'b list) parser =
  (fun input_list ->
     let rec loop outputs inputs =
       let result = parser inputs in
       match result with
       | ParseFailure (_, _) -> outputs, inputs
       | ParseSuccess (r, rest) ->
           (loop (outputs@[r]) rest)
     in
     let tokens_list, rest = loop [] input_list in
     ParseSuccess(tokens_list, rest))
;;

let many1 parser =
  (parser <*> (many parser)) >>>
  (fun (x, xs) -> x::xs)
;;

let parse (parser: ('input, 'output) parser) (input_list: 'input list): 'output =
  match parser input_list with
  | ParseSuccess (result, leftover) ->
    begin
      match leftover with
      | [] -> result
      | _ -> failwith "Error: parsing finished before EOF"
    end
  | ParseFailure (expected, (line, col, _, _)) ->
    failwith (sprintf "Error @ (%d:%d): expected %s" line col expected)
;;

let fix f =
  let rec p = lazy (f r)
  and r = (fun input_list ->
    Lazy.(force p) input_list)
  in
  r
