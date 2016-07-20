open Bool3
open Random

(* Definition of static join points and boolean pcd on them *)
(* that can be used for aspects deployement *)

type host = Unix.sockaddr

type extra_jp_info = host

type channel_info = string

type reaction_info = channel_info list

type static_jp = extra_jp_info * reaction_info

type pcd = 
  | PCTrue 
  | PCFalse 
  | Contains of reaction_info
  | Host of host
  | IsHost of host
  | And of pcd * pcd 
  | Not of pcd
  | CausedBy of pcd

let create_host ip = Unix.ADDR_INET(Join.Site.get_local_addr(),ip)

let create_extra_jp ip = create_host ip

let create_static_jp h p = (h , p)

let test_static_joinpoint pcd jp =
  let rec test_static_joinpoint' pcd jp =
    match pcd with 
      | PCTrue          -> True 
      | PCFalse         -> False
      | IsHost(h)       -> (match jp with (h',_) -> bool_to_bool3 (h = h'))
      | Host(h)         -> True
      | Contains(p)     -> 
	(match jp with (_,p') -> bool_to_bool3 (List.for_all (fun s -> List.exists (fun s' -> s = s') p') p))
      | And(pcd1,pcd2)  -> and3 (test_static_joinpoint' pcd1 jp) (test_static_joinpoint' pcd2 jp)
      | Not(pcd1)       -> not3 (test_static_joinpoint' pcd1 jp)
      | CausedBy(pcd)   -> Unknown
  in bool3_to_bool (test_static_joinpoint' pcd jp)

(* Definition of dynamic join points. For the moment, a dynamic joinpoint *) 
(* is just a tuple of arguments, but other dynamic information may be collected. *)

type dynamic_jp_channel = 
  | JP : channel_info * 'a -> dynamic_jp_channel

  (* create a dynamic join point from the arguments of the join point *)

let create_dynamic_jp_channel ch arg = JP (ch,arg)

  (* get arguments from the dynamic jp, uses Obj.magic to get the type back *)

type dynamic_channel = 
  | Chan : channel_info * 'a -> dynamic_channel

type dynamic_jp = 
  | DJP : extra_jp_info * dynamic_channel list *  dynamic_jp_channel list -> dynamic_jp

let create_dynamic_jp ext obj jp = DJP (ext, obj,jp)

let jp_to_arg channel_info (DJP (_,_,djp)) = 
  let rec jp_to_arg_rec channel_info = function
    | [] -> raise Not_found 
    | JP (channel_info',arg) :: djp ->
      if channel_info = channel_info' 
      then Obj.magic arg
      else jp_to_arg_rec channel_info djp
  in jp_to_arg_rec channel_info djp

let jp_to_obj (DJP (_,obj,_)) = obj

let rec obj_to_chan channel_info = function
    | [] -> raise Not_found 
    | Chan (channel_info',chan) :: q ->
      if channel_info = channel_info' 
      then Obj.magic chan
      else obj_to_chan channel_info q


(* Definition of the dynamic control flow which is a tree *)
(* of all triggering join points *)

type control_flow = 
    EmptyFlow 
  | Node of dynamic_jp * control_flow list

let add jp cflow = Node (jp, cflow)

(* Definition of OR an a list of booleans *)

let rec or_list l = 
  match l with
    | [] -> false
    | h ::t -> h || or_list t

(* booleans for pcs *)

(* let shuffle = List.fold_right (fun l ls -> List.map (List.append l) ls)  *)

let rec shuffle ls = function 
  | [] -> []
  | l' :: ls' -> List.map (fun l -> l @ l') ls @ shuffle ls ls'
  

let rec get_dynamic_joinpoint_list pcd jp cflow =
    (* Check if a pointcut occurs in the control flow *)
    
  let rec is_caused_by pcd = function
    | EmptyFlow -> []
    | Node(jp,cflow_list) -> get_dynamic_joinpoint_list pcd jp (Node(jp,cflow_list)) @
      List.concat (List.map (is_caused_by pcd) cflow_list)
  in
  
  (* Check if a pointcut occurs directly in the control flow *)
  
  match pcd with 
  | PCTrue          -> [[]] 
  | PCFalse         -> []
  | IsHost(h)       -> (match jp with (DJP(h',_,_)) -> if (h = h') then [[]] else [])
  | Host(h)         -> [[jp]]
  | Contains(p)     -> (match jp with (DJP (_,_,p')) -> 
    if (List.for_all (fun s -> List.exists (function JP(s',_) -> s = s') p') p)
    then (print_string(""); flush stdout;[[jp]])
    else (print_string(""); flush stdout; []))
  | And(pcd1,pcd2)  -> shuffle (get_dynamic_joinpoint_list pcd1 jp cflow) (get_dynamic_joinpoint_list pcd2 jp cflow)
  | Not(pcd1)       -> (match (get_dynamic_joinpoint_list pcd1 jp cflow) with 
    | [] -> [[]] 
    | _  -> [])
  | CausedBy(pcd) -> (match cflow with
    | Node(_,cflow_list) -> List.concat (List.map (is_caused_by pcd) cflow_list)
    (* this case should be unused *)
    | EmptyFlow -> [])

let select_from_list l = let _ = Random.self_init () in
			 let n = Random.int (List.length l) in
			 List.nth l n

(* printers for debugging *)

let print_list printer l =
  let rec rec_print_list l =
    match l with 
      | [] -> ()
      | h :: t -> printer h; print_string (" "); rec_print_list t
  in print_string ("[ "); rec_print_list l; print_string ("]")	   

let string_of_extra_jp_info e = match e with 
  | Unix.ADDR_UNIX(s) -> s
  | Unix.ADDR_INET((h,n)) -> Unix.string_of_inet_addr h ^ " , " ^ string_of_int n

let rec print_static_jp jp = match jp with (h,l) -> print_list print_string l; print_string ((string_of_extra_jp_info h) ^ " @ ")
