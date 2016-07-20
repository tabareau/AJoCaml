open Join_point

(* Dummy type "any list" to enable any type for advices *)
(* We use the fact that communications with the name server are untyped *)
(* Note that we can't use a polymorphic type " 'a list " because *)
(* the types of channel registered on NS cannot be polymorphic *)

type any = unit

class aspect_list (init: (any * pcd) list) = 
object (self)
  val mutable list = init
  method add elt = list <- list @ [elt]
  method get = list
end  

(* (\* Dummy type " (any Join.chan) list " to enable any type for advices *\) *)
(* (\* instead of " ('a Join.chan) list ", same reason *\) *)

(* class weaver_list (init: ((any * pcd)  Join.chan * static_jp)list) =  *)
(* object (self) *)
(*   val mutable list = init *)
(*   method add elt = list <- list @ [elt] *)
(*   method get = list *)
(* end   *)


(********************************)
(* Definition of weaving registries *)
(********************************)

(* When an object is instantiated, its local weaver is added to the list weaver_list *)
(* all deployed aspects can advise the reaction of throw the weaver *)
(* When an aspect is deployed, it is added to the list aspect_list *)

class weaving_registry (ip : int) =
  let weaving_registry aspect_list =
    def get_aspect(k) = k (aspect_list#get) (* (List.map (fun (advice,pc) ->  *)
      (* if (test_static_joinpoint pc jp) then (advice,pc)) *)
      (* aspect_list#get) *)
     or deploy(asp) = aspect_list#add(asp) ; 0
  in (get_aspect,deploy) in
object (self)
  val mutable aspect_list = new aspect_list []
  method get_aspect = (fst (weaving_registry aspect_list))
  method deploy = (snd ((weaving_registry aspect_list)) : (any * pcd) Join.chan)
  initializer let ns = Connection.ns_server ip in
	      Join.Ns.register ns ("get_aspect" ^ string_of_int ip) 
		                  (self#get_aspect : ((any * pcd) list Join.chan) Join.chan) ;
	      Join.Ns.register ns ("deploy" ^ string_of_int ip) 
		                  (self#deploy : (any * pcd) Join.chan) 
end



(* When an object is instantiated, its local weaver is added to the list weaver_list *)
(* all deployed aspects can advise the reaction of throw the weaver *)
(* When an aspect is deployed, it is added to the list aspect_list *)
(* all instanted objectscan can be advised by this aspect *)

(* class weaving_registry_history (ip : int) = *)
(*   let weaving_registry weaver_list aspect_list = *)
(*     def add_weaver(add_aspect,jp) = weaver_list#add((add_aspect,jp)) ; *)
(*                                     List.iter (fun (advice,pc) ->  *)
(* 				      if (test_static_joinpoint pc jp) then spawn add_aspect(advice,pc)) aspect_list#get; 0 *)
(*      or deploy(advice,pc) = aspect_list#add((advice,pc)) ; *)
(*                          List.iter (fun (add_aspect,jp) ->  *)
(* 			   if (test_static_joinpoint pc jp) then spawn add_aspect(advice,pc)) weaver_list#get; 0 *)
(*   in (add_weaver , deploy) in *)
(* object (self) *)
(*   val mutable weaver_list = new weaver_list [] *)
(*   val mutable aspect_list = new aspect_list [] *)
(*   method add_weaver = fst (weaving_registry weaver_list aspect_list) *)
(*   method deploy = (snd (weaving_registry weaver_list aspect_list) :  (any * pcd) Join.chan) *)
(*   initializer let ns = Connection.ns_server ip in *)
(* 	      Join.Ns.register ns ("add_weaver" ^ string_of_int ip) (self#add_weaver : ((any * pcd) Join.chan * static_jp) Join.chan) ; *)
(* 	      Join.Ns.register ns  ("deploy" ^ string_of_int ip) (self#deploy : (any * pcd) Join.chan) *)
(* end *)

(* Closed registry, the deploy channel is owned by creator but not publicly registered *)

(* class weaving_registry_closed (ip : int) = *)
(*   let weaving_registry_closed aspect_list = *)
(*     def add_weaver(add_aspect,jp) = List.iter (fun (advice,pc) ->  *)
(*       if (test_static_joinpoint pc jp) then spawn add_aspect(advice,pc)) aspect_list#get; 0 *)
(*      or deploy(asp) = flush stdout; aspect_list#add(asp) ; 0 *)
(*   in (add_weaver,deploy) in *)
(* object (self) *)
(*   val mutable aspect_list = new aspect_list [] *)
(*   method add_weaver = (fst (weaving_registry_closed aspect_list)) *)
(*   method deploy = (snd ((weaving_registry_closed aspect_list)) : (any * pcd) Join.chan) *)
(*   initializer let ns = Connection.ns_server ip in *)
(* 	      Join.Ns.register ns ("add_weaver" ^ string_of_int ip)  *)
(* 		               (self#add_weaver : ((any * pcd) Join.chan * static_jp) Join.chan) *)
(* end *)
