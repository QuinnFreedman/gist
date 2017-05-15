open Batteries;;
open Printf;;
open Types;;
open StringMap;;

let generate_error s p =
  let (line, col, _, _) = p in
  sprintf "Error @ (%d:%d): %s" line col s
;;

type expr_type =
  | TypeFunction of int
  | TypeVar
;;

let check_correctness (e: expr): string list =
  let rec inner e env =
    match e with
    | Var(s, p) ->
      if StringMap.mem s env then
        []
      else
        [generate_error (sprintf "variable `%s` is not defined here" s) p]
    | Let(s, e1, e2, p) ->
      (inner e1 env)@
      (inner e2 (StringMap.add s TypeVar env))
    | _ -> failwith "todo"
  in
  inner e StringMap.empty
;;
