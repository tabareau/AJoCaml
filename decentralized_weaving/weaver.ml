open Join_point

(***********************************)
(* Definition of different weavers *)
(***********************************)

(* Advisable reaction : the weaver applies all current advices asynchronously  *)
(* and passes proceed to each advice *)

class advisable_weaver (p:reaction_info) (ip:int) =
  (* let jp_s = create_static_jp (create_extra_jp ip) p in *)
  let get_aspect = Connection.lookup ("get_aspect" ^ string_of_int ip) ip in  
  let weaver =
    def weave(cflow,proceed,jp_d) = 
      def aspect_list(aspL) =
      let advs = List.filter 
        (fun (_,jps) -> jps != [])
        (List.map (fun (adv,pc) -> (adv,get_dynamic_joinpoint_list pc jp_d cflow)) aspL)
      in
      if (advs != [])
      then  (List.iter (fun (adv,jps) -> spawn adv(cflow,proceed,jp_d,select_from_list jps)) advs ; 0)
      else  proceed(cflow,jp_d)
  in get_aspect(aspect_list)
  in weave in
object (self)
  (* val mutable aspect_list = new aspect_list [] *)
  method weave = (weaver : (control_flow * ((control_flow * dynamic_jp) Join.chan) * dynamic_jp) Join.chan)
end


class observable_weaver (p:reaction_info) (ip:int) =
  (* let jp_s = create_static_jp (create_extra_jp ip) p in *)
  let get_aspect = Connection.lookup ("get_aspect" ^ string_of_int ip) ip in  
  let weaver =
    def weave(cflow,proceed,jp_d) = 
    def aspect_list(aspL) =
    List.iter
      (fun (advice,pc) -> let jps = get_dynamic_joinpoint_list pc jp_d cflow in
			  if (List.length jps != 0)
                          then spawn advice(cflow,jp_d,select_from_list jps)) aspL;
    proceed(cflow,jp_d)
    in get_aspect(aspect_list)
  in weave in
object (self)
  (* val mutable aspect_list = new aspect_list [] *)
  method weave = ( weaver : (control_flow * ((control_flow * dynamic_jp) Join.chan) * dynamic_jp) Join.chan)
end

(* Opaque reaction : the weaver skips advices and direclty calls proceed *)

class ['a] opaque_weaver (p:reaction_info) (ip:int) =
  (* let jp_s = create_static_jp (create_extra_jp ip) p in *)
  let weaver =
    def weave(cflow,proceed,jp_d) = proceed(cflow,jp_d)
  in weave in
object (self)
  method weave = (weaver : (control_flow * ((control_flow * dynamic_jp) Join.chan) * dynamic_jp) Join.chan)
end



(* class observable_weaver (p:reaction_info) (ip:int) = *)
(*   let jp_s = create_static_jp (create_extra_jp ip) p in *)
(*   let weaver aspect_list = *)
(*     def add_aspect(asp) = aspect_list#add(asp) ; 0 *)
(*       (\* or weave(cflow,proceed,jp_d) = List.iter  *\) *)
(*       or weave(cflow,proceed,jp_d) = List.iter *)
(*     (fun (advice,pc) -> let jps = get_dynamic_joinpoint_list pc jp_d cflow in *)
(* 			if (List.length jps != 0) *)
(*                         then spawn advice(cflow,jp_d,select_from_list jps)) aspect_list#get; *)
(*     proceed(cflow,jp_d) *)
(*   in (add_aspect , weave) in *)
(* object (self) *)
(*   val mutable aspect_list = new aspect_list [] *)
(*   method add_aspect = fst (weaver aspect_list) *)
(*   method weave = (snd (weaver aspect_list) : (control_flow * ((control_flow * dynamic_jp) Join.chan) * dynamic_jp) Join.chan) *)
(*   (\* register the weaver to the broadcast *\) *)
(*   initializer let add_weaver = Connection.lookup ("add_weaver" ^ string_of_int ip) ip *)
(* 	      in  spawn add_weaver(self#add_aspect,jp_s) *)
(* end *)
