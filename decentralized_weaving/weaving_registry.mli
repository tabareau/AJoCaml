open Join_point

type any = unit

class aspect_list :
  (any * pcd) list ->
  object
    val mutable list : (any * pcd) list
    method add : any * pcd -> unit
    method get : (any * pcd) list
  end

(* class weaver_list : *)
(*   ((any * pcd) Join.chan * static_jp) list -> *)
(*   object *)
(*     val mutable list : ((any * pcd) Join.chan * static_jp) list *)
(*     method add : (any * pcd) Join.chan * static_jp -> unit *)
(*     method get : ((any * pcd) Join.chan * static_jp) list *)
(*   end *)

class weaving_registry :
  int ->
  object
    val mutable aspect_list : aspect_list
    method get_aspect : ((any * pcd) list Join.chan) Join.chan
    method deploy : (any * pcd) Join.chan
  end

(* class weaving_registry_history : *)
(*   int -> *)
(*   object *)
(*     val mutable aspect_list : aspect_list *)
(*     (\* val mutable weaver_list : weaver_list *\) *)
(*     method add_weaver : ((any * pcd) Join.chan * static_jp) Join.chan *)
(*     method deploy : (any * pcd) Join.chan *)
(*   end *)

(* class weaving_registry_closed : *)
(*   int -> *)
(*   object *)
(*     val mutable aspect_list : aspect_list *)
(*     method add_weaver : ((any * pcd) Join.chan * static_jp) Join.chan *)
(*     method deploy : (any * pcd) Join.chan *)
(*   end *)
