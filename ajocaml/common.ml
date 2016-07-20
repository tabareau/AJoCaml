(********************************************************************)
(* SessML - Implementation                                          *)
(*                                                                  *)
(* common.ml                                                        *)
(********************************************************************)
(* Time-stamp: <02-08-2010 16:27 by Denielou Malo> *)

(* Debug functions *)

let debug_bool = true
let debug_main = true
let debug_parserlexer = true
let debug_generation = true
let debug_wf = true
let debug_properties = false
let debug_graph = true
let debug_visibility = true


let gen_debug d =
  if d then
    (fun modul msg -> begin
      print_string ("["^modul^"] ");
      print_string (msg);
      print_newline ();
      flush stdout
    end)
  else fun _ _ -> ()

let debug_common = gen_debug debug_bool "Common"

(* Positionning *)

type pos = int * int
type info = pos * pos

let bogusInfo = ((0,0),(0,0))

let info_to_string ((l1,c1),(l2,c2)) =
(*  let s = Printf.sprintf "line %d, char %d, to line %d, char %d " l1 c1 l2 c2 in  *)
let s = "line"^(string_of_int l1) in
(*  debug_common s ; *)
  s

let merge_info (a,_) (_,d) = (a,d)


(* Errors *)
exception Syntax_error of string*info
exception Parse_error of string*info


(*********************)
(* List Manipulation *)
(*********************)

(* Substitution *)
let alias subst =
  function n ->
    if List.mem_assoc n subst 
    then List.assoc n subst
    else n

(* Remove duplicates in a list *)
let rec remove_duplicates l = 
  match  l with
      [] -> []
    | a::q -> if (List.mem a q) then (remove_duplicates q) else a::(remove_duplicates q)

(* List of assoc in an assoc list*)
let rec subset_assoc x = function
    [] -> []
  | (a,b)::q -> if a = x then b::(subset_assoc x q) else subset_assoc x q

let safe_assoc x l = 
  if List.mem_assoc x l
  then List.assoc x l
  else []


(* Substraction *)
let rec substract l ll =
  let rec remove x = function
      [] -> []
    | a::q -> if a = x then remove x q else a::(remove x q) in
  match l with
      [] -> ll
    | a::q -> substract q (remove a ll)

let rec substract_fst l ll =
  let rec remove x = function
      [] -> []
    | a::q -> if fst a = x then remove x q else a::(remove x q) in
  match l with
      [] -> ll
    | a::q -> substract_fst q (remove a ll)

let rec intersect l ll =
  match l with
      [] -> []
    | a::q -> if List.mem a ll then a::(intersect q ll) else intersect q ll

(* product *)
let rec product l ll =
  match l with
      [] -> []
    | a::q -> (List.map (fun x -> a,x) ll)@(product q ll)

let extract_fst l = List.map fst l
let extract_snd l = List.map snd l

let rec print_list symbol = function
  | [] -> ""
  | name :: q -> match q with 
      | [] -> name 
      | _ -> name ^ symbol ^ print_list symbol q


(***********************)
(* String Manipulation *)
(***********************)

let to_s = string_of_int
let concat sep = String.concat sep 
let concat_s = concat ";"
let concat_c = concat ","
  (* let msg_to_s m = Printf.sprintf "(%s,{%s},%s)" c (ls_to_s_comma v) p*)

let capitalize = String.capitalize


(* String parsing *)

let split s =
  if String.contains s '.'
  then
    let n = String.length s in
    let i = String.index s '.' in
    (String.sub s 0 i,String.sub s (i+1) (n-1-i))
  else
    ("",s)
