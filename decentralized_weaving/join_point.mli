type host = Unix.sockaddr

type extra_jp_info = host

type channel_info = string (* string * version *)

type reaction_info = channel_info list

type static_jp = extra_jp_info * reaction_info

type pcd =
  | PCTrue 
  | PCFalse 
  | Contains of reaction_info
  | Host of host
  | IsHost of host
  | And of pcd * pcd
  (* | Or of pcd * pcd *)
  | Not of pcd
  (* | ProducedBy of pcd *)
  | CausedBy of pcd
  (* | InThread of pcd * pcd *)

val create_host : int -> host

val create_extra_jp : int -> extra_jp_info

val create_static_jp : extra_jp_info -> reaction_info -> static_jp

val test_static_joinpoint : pcd -> static_jp -> bool

type dynamic_jp_channel = 
  | JP : channel_info * 'a -> dynamic_jp_channel

val create_dynamic_jp_channel : channel_info -> 'a -> dynamic_jp_channel

type dynamic_channel = 
  | Chan : channel_info * 'a -> dynamic_channel

type dynamic_jp = 
  | DJP : extra_jp_info * dynamic_channel list * dynamic_jp_channel list -> dynamic_jp

val create_dynamic_jp : extra_jp_info -> dynamic_channel list ->  dynamic_jp_channel list -> dynamic_jp

val jp_to_arg : channel_info -> dynamic_jp -> 'a

val jp_to_obj : dynamic_jp -> dynamic_channel list

val obj_to_chan : channel_info -> dynamic_channel list -> 'a

(* val change_jp : channel_info -> 'a -> dynamic_jp -> dynamic_jp *)

type control_flow =
  | EmptyFlow
  | Node of dynamic_jp * control_flow list

(* val test_dynamic_joinpoint : pcd -> dynamic_jp -> control_flow -> bool *)

val get_dynamic_joinpoint_list : pcd -> dynamic_jp -> control_flow -> (dynamic_jp list) list

val select_from_list : 'a list -> 'a

val add : dynamic_jp -> control_flow list -> control_flow

val print_static_jp : static_jp -> unit

(* val print_control_flow : control_flow -> unit *)
