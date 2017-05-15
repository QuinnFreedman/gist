(*
  This code creates a "module" which defines a dictionary data type with keys
  of type string.  The syntax below is probably pretty alien and you don't need
  to understand it to use the data structure.  It's enough to know that the
  members on the module are documented here:

    http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatMap.S.html

  You can refer to these members by prefixing them with this module's name:
  StringMap.  For instance, you may write

    let m = StringMap.add "x" 5 (StringMap.empty) in
    StringMap.find "x" m

  to create a single-mapping dictionary called m and then look up the key "x" in
  that dictionary.  See the documentation above for more StringMap functions.
*)
module StringMap = Map.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end);;

module StringSet = Set.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end
  );;

(** This function takes a list of (key, value) pairs and returns a map containg
    those bindings plus a list of all the duplicate keys *)
let map_from_bindings_list (bindings:(string * int) list): int StringMap.t * string list = 
  let rec map_from_bindings_list_inner (bindings:(string * int) list) 
                                       (map: int StringMap.t) 
                                       (duplicate_keys: string list): 
                                       int StringMap.t * string list = 
    match bindings with
    | [] -> map, duplicate_keys
    | (key, value)::rest -> 
      if StringMap.mem key map then
        map_from_bindings_list_inner rest map (duplicate_keys@[key])
      else
        let new_map = StringMap.add key value map in
        map_from_bindings_list_inner rest new_map duplicate_keys
  in

  map_from_bindings_list_inner bindings StringMap.empty []
;;

(** This function takes a list of strings and returns a set containg
    those strings plus a list of all the duplicate strings *)
let set_from_list ?(starting_map: StringSet.t=StringSet.empty) (keys: string list): StringSet.t * string list = 
  let rec set_from_list_inner (keys: string list) (map: StringSet.t) (duplicate_keys: string list): StringSet.t * string list =
    match keys with
    | [] -> map, duplicate_keys
    | first::rest -> 
        let new_map = StringSet.add first map in
      if StringSet.mem first map then
        set_from_list_inner rest new_map (duplicate_keys@[first])
      else
        set_from_list_inner rest new_map duplicate_keys
    in

  set_from_list_inner keys starting_map []
;;