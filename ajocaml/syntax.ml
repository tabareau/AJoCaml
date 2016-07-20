(********************************************************************)
(* Aspect JoCaml - Implementation                                   *)
(*                                                                  *)
(* syntax.ml                                                          *)
(********************************************************************)
(* Time-stamp: <05-08-2011 09:19 by Nicolas Tabareau> *)

open Common

type principal = string

type vars = string list

type as_self = string

type react_kind = 
  | Opaque
  | Advisable
  | Synchronous
  | Observable

type react_name = string

type react_registry = string

type as_chan = string

type as_type = string * string

type arg = string

type code = string

type as_pointcut =
  | PCTrue_ 
  | PCFalse_ 
  | Contains_ of string * (string * string list) list
  | Host_ of string
  | IsHost_ of string
  | And_ of as_pointcut * as_pointcut
  | Not_ of as_pointcut
  | CausedBy_ of as_pointcut

type as_binder =
  | ContainsB of string * (string * string list) list
  | HostB of string

type react_side = (as_chan * arg list ) list

type as_react =
    (react_name * react_registry * react_kind * react_side * (code*react_side)) list

type as_private_chan = (as_chan * as_type) list 

type as_public_chan = (as_chan * as_type) list

type as_object_type = Dobject of 
    as_self * as_react * as_private_chan * as_public_chan * code

type polytype = string list

type args_class = (string * string) list

type as_class_type = Dclass of string * polytype * args_class * code * as_object_type

type pattern_type = (string * string list) list

type as_aspect_type = Daspect of string * args_class * as_pointcut * code * react_side

let emptyClass = Dclass( "", [] , [] , "", Dobject ("", [] , [] , [] , ""))

let emptyAspect = Daspect( "", [] , PCTrue_, "", [])

type ast = (as_class_type * as_aspect_type * code) list

let rec deploy_weaver = 
  let weaver_kind = function 
    | Opaque -> "new Weaver.opaque_weaver "
    | Advisable -> "new Weaver.advisable_weaver "
    | Synchronous -> "new Weaver.synchronous_weaver " 
    | Observable -> "new Weaver.observable_weaver " 
  in function 
    | [] -> ""
    | (name,ip,kind,left,_) :: q -> 
      "    let " ^ name ^ " = " ^ weaver_kind kind ^ "[\"" ^ 
	print_list "\";\"" (extract_fst left) ^ "\"] " ^ ip ^ " in \n" ^
	deploy_weaver q

let rec print_react_right2 = function
  | [] -> ""
  | (channel,args) :: q -> 
    let s = 
      let start = channel ^ "(cflow" in
      match args with 
      | [] -> start ^ ")"
      | _ -> start ^ "," ^ print_list "," args ^ ")" 
    in match q with 
    | [] -> s ^ ""
    | _ -> s ^ " & " ^ print_react_right2 q 

let rec print_react_right = function
  | [] -> "selfCh(myself)"
  | (channel,args) :: q -> 
    let s = 
      let start = channel ^ "(cflow" in
      match args with 
      | [] -> start ^ ")"
      | _ -> start ^ "," ^ print_list "," args ^ ")" 
    in match q with 
    | [] -> s ^ " & selfCh(myself)"
    | _ -> s ^ " & " ^ print_react_right q 

let rec print_reaction private_chan = 
  let rec print_react_left n = function
    | [] -> "selfCh(myself)"
    | (channel,args) :: q -> 
      let s = 
	let start = channel ^ "(cflow" ^ string_of_int n in
	match args with 
	  | [] -> start ^ ")"
	  | _ -> start ^ "," ^ print_list "," args ^ ")" 
      in match q with 
	| [] -> s ^ " & selfCh(myself)"
	| _ -> s ^ " & " ^ print_react_left (n+1) q in
  let rec gen_cflow n = function 
    | [] -> "" 
    | _ :: q -> 
      let s = "cflow" ^ string_of_int n  
      in match q with 
	| [] -> s
	| _ -> s ^ ";" ^ gen_cflow (n+1) q
   in function 
    | [] -> ""
    | (name,ip,_,left,right) :: q -> 
      let proceed = "proceed_" ^ name in 
      let public_jp = substract_fst private_chan left in
      let print_public_jp_in = function 
	| (chan,jp) -> "Join_point.create_dynamic_jp_channel \"" ^ chan ^ "\" (" ^ print_list "," jp ^ ") " in 
      let public_jp_string_in = List.map print_public_jp_in public_jp in
      let print_public_jp_out = function 
	| (chan,jp) -> "(" ^ print_list "," jp ^ ") = Join_point.jp_to_arg \"" ^ chan ^ "\" dyn_jp" in 
      let public_jp_string_out = List.map print_public_jp_out public_jp in
      let is_or = match q with [] -> "" | _ ->  "\t or \n" in
      let (code_right,right) = right in
      "\t   " ^ print_react_left 1 left ^ " = def " ^
	proceed ^ "(cflow,dyn_jp) = " ^ (if public_jp_string_out != [] 
	  then (*""*) "let " ^ print_list " and " public_jp_string_out ^ " in"
	  else "") ^ "\n" 
        ^ code_right ^ print_react_right right ^ "\n" ^
	"\t \t in let djp = create_dynamic_jp (Join_point.create_extra_jp "^ ip ^ ") myself [ " ^ print_list ";" public_jp_string_in ^ "]\n" ^
	"\t \t in let cflow = Join_point.add djp ["  ^gen_cflow 1 left ^ "]" ^ " in\n" ^
	"\t \t" ^ name ^ "#weave(cflow," ^ proceed ^ ", djp)\n" ^ 
	is_or ^ print_reaction private_chan q

let rec print_method = function 
  | [] -> ""
  | (name,as_type) :: q -> 
    let chan = match as_type with
      | (_ , "") -> name
      | (t1,t2)  -> "(" ^ name ^ " : ( _ * " ^ t1 ^ ") " ^ t2 ^ ")"
    in
    "method " ^ name ^ " = " ^ chan ^ "\n" ^ print_method q
 
let rec print_chan_obj = function
  | [] -> ""
  | [h] -> "Chan(\"" ^ h ^ "\",self#" ^ h ^ ")"
  | h :: q -> "Chan(\"" ^ h ^ "\",self#" ^ h ^ "); " ^ print_chan_obj q

let print_obj = function | Dobject (self,as_react,as_priv,as_pub,code) -> 
    let all_list = (extract_fst as_priv @ extract_fst as_pub) in 
    let pub_list = (extract_fst as_pub) in 
    let chan_list = print_list " , " all_list ^ " , selfCh" in
    let init = if (String.compare code "" == 0) 
               then "initializer spawn selfCh([" ^ print_chan_obj pub_list ^ "]) " 
               else  "    " ^ code ^ "; spawn selfCh([" ^ print_chan_obj pub_list ^ "]) \n" in
    "let " ^ chan_list ^ " = \n" ^ deploy_weaver as_react ^ 
      "\t def\n" ^ print_reaction (extract_fst as_priv) as_react ^ 
      "\t \t in (" ^ chan_list ^ ") in\n" ^ 
      "object(" ^ self ^ ")\n" ^ print_method as_pub ^ init
      

let print_polytype = function 
  | [] -> ""
  | t -> "[" ^ print_list "," t ^ "] "
 
let rec print_args_class = function 
  | [] -> ""
  | (arg,typ) :: q ->  if (String.compare typ "" == 0) 
    then arg ^ " " ^ (print_args_class q) 
    else "(" ^ arg ^ ":" ^ typ ^ ") " ^ (print_args_class q)

let print_class = function | Dclass (name,class_type,args,code,d_object) ->
  if (String.compare name "" == 0) then "" else
  "class " ^ print_polytype class_type ^ name ^ " " ^ print_args_class args ^ " =\n" 
  ^ code ^ "\n" ^ print_obj d_object ^ "end \n"

let rec print_pattern = function
  | [] -> ""
  | [(name,bvariables)]      -> name
  | (name,bvariables) :: q -> name ^ " ; " ^ print_pattern q

let rec print_bind_pattern n = function
  | [] -> ""
  | [(name,bvariables)]    -> "let (" ^ print_list "," bvariables ^ ") = jp_to_arg " ^ name ^ " (List.nth jps " ^ string_of_int n ^ ") in "
  | (name,bvariables) :: q -> "let (" ^ print_list "," bvariables ^ ") = jp_to_arg " ^ name ^ " (List.nth jps " ^ string_of_int n ^ ")in " ^ print_bind_pattern n q

let rec convert_pc = function 
  | PCTrue_ -> []
  | PCFalse_ -> []
  | Contains_ (obj_name, pattern) -> [ContainsB (obj_name, pattern)]
  | Host_ h -> [HostB h]
  | IsHost_ _ -> []
  | And_ (pc1,pc2) -> convert_pc pc1 @ convert_pc pc2
  | Not_ _ -> []
  | CausedBy_ pc -> convert_pc pc

let rec print_pc_bound n = function 
  | [] -> ""
  | ContainsB (obj_name,pattern) :: pcs -> "let " ^ obj_name ^ " = jp_to_obj (List.nth jps " ^ string_of_int n ^ ") in " ^ print_bind_pattern n pattern ^ print_pc_bound (n+1) pcs
  | HostB h :: pcs -> "let " ^ h ^ " = jp_to_obj (List.nth jps " ^ string_of_int n ^ ") in " ^ print_pc_bound (n+1) pcs

let rec print_pc = function 
  | PCTrue_ -> "PCTrue"
  | PCFalse_ -> "PCFalse"
  | Contains_ (obj_name, pattern) -> "Contains [" ^ print_pattern pattern ^ "]"
  | Host_ h -> "Host " ^ h
  | IsHost_ h -> "IsHost " ^ h
  | And_ (pc1,pc2) -> "And (" ^ print_pc pc1 ^ "," ^ print_pc pc2 ^ ")" 
  | Not_ pc1 -> "Not (" ^ print_pc pc1 ^  ")"  
  | CausedBy_ pc1 -> "CausedBy (" ^ print_pc pc1 ^  ")"

let print_aspect = function | Daspect (name,args,pc,adv_code,adv_react) ->
  if (String.compare name "" == 0) then "" else
  "class ['a] " ^ name ^ "_advice " ^ print_args_class args ^ " =\n" ^
  "let advice =\n" ^ 
    " def advice(cflow,proceed,jp,jps) = " ^ print_pc_bound 0 (convert_pc pc) ^ adv_code ^ print_react_right2 adv_react ^ "\n in (advice) in \n" ^
    "object(self) \n" ^
    "  method advice = (advice : ( control_flow * 'a * dynamic_jp * dynamic_jp list) Join.chan) \n" ^ 
       "end \n\n" ^   
  "let " ^ name ^ " " ^ print_args_class args ^ " = let my_adv = new " ^ name ^ "_advice " ^
    print_args_class args ^ " in \n" ^ 
    "Aspect.aspect ip (" ^ print_pc pc ^ ") my_adv#advice \n"

let translate ast = "open Join_point\n\n" ^ 
  print_list "\n" (List.map (function | (c,a,s) -> (print_class c) ^ (print_aspect a) ^s) ast)


(* type node = int  *)

(* let node_ident =  *)
(*   Hashtbl.create 10 *)
    
(* let clear_ident () =  *)
(*   Hashtbl.clear node_ident *)

    
(* let find_node (i:string) = *)
(*   Hashtbl.find node_ident i *)
    
(* let remove_ident (i:string) = *)
(*   Hashtbl.remove node_ident i *)

(* let nref = ref 0 *)

(* let create_node_name ()= *)
(*   let i = !nref in  *)
(*   let _ = incr nref in *)
(*   i *)

(* let add_ident (i:string) = *)
(*   if Hashtbl.mem node_ident i *)
(*   then Hashtbl.find node_ident i *)
(*   else  *)
(*     let n = create_node_name () in *)
(*     let () = Hashtbl.add node_ident i n in *)
(*     n *)
