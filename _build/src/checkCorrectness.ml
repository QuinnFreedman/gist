open Batteries;;
open Printf;;
open Types;;
open StringMap;;

let generate_error s p =
  let (line, col, _, _) = p in
  sprintf "Error @ (%d:%d): %s" line col s
;;

let check_correctness (e: expr): string list =
  let rec inner e env =
    match e with
    | Var(s, p) ->
      if StringMap.mem s env then
        []
      else
        [generate_error (sprintf "variable `%s` is not defined here" s) p]
    | _ -> []
  in
  inner e StringMap.empty
;;
