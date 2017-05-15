open Batteries;;
open Printf;;

type position = int * int * int * int;;

type token =
  | LPAREN of bool * position
  | RPAREN of position
  | INT    of int * position
  | IDENT  of bool * string * position
;;

let position_of_token t =
  match t with
  | IDENT(_, _, p) -> p
  | INT(_, p) -> p
  | LPAREN(_, p) -> p
  | RPAREN(p) -> p
;;

let token_to_str x =
   match x with
   | IDENT (_, s, _) -> s
   | _ -> failwith "error"
;;

let token_to_int x = 
   match x with
   | INT (s, _) -> s
   | _ -> failwith "error"
;;

type expr =
  | Int of int * position
  | Var of string * position
  | Call of string * expr list * position
  | Let of string * expr * expr * position 
  | Defun of string * string list * expr * position
;;

let rec string_of_expr e =
  match e with
  | Int(i, _) -> sprintf "Int(%d)" i
  | Var(s, _) -> sprintf "Var(%s)" s
  | Call(s, lst, _) -> 
    sprintf "Call(%s %s)" s 
      (lst |> List.map string_of_expr |> String.concat " ")
  | Let(s, e1, e2, _) -> 
    sprintf "Let(%s %s %s)" s (string_of_expr e1) (string_of_expr e2)
  | Defun(s, args, e, _) ->
    sprintf "Defun(%s %s %s)" s (String.concat " " args) (string_of_expr e)

