open Join_point

(***********************************)
(* Definition of different aspects *)
(***********************************)

(* An aspect is constituted by a poincut and an advice *)

(* class aspect ip pc adv = *)
(*   let advice =  *)
(*     def advice(cflow,proceed,jp_d) = if (test_dynamic_joinpoint pc jp_d cflow) then adv(cflow,proceed,jp_d) else proceed(cflow,jp_d) *)
(*   in advice in *)
(* object(self) *)
(*   method advice = (advice:(control_flow * ((control_flow * dynamic_jp) Join.chan) * dynamic_jp) Join.chan) *)
(* (\* register the advice to the broadcast using an initializer *\) *)
(*   initializer let deploy = Connection.lookup ("deploy" ^ string_of_int ip) ip *)
(* 	      in  spawn deploy(self#advice,pc) *)
(* end *)

let aspect ip pc adv =
(* register the advice to the broadcast using an initializer *)
  let deploy = Connection.lookup ("deploy" ^ string_of_int ip) ip
  in  spawn deploy(adv,pc)

let aspect_before ip pc adv =
(* register the advice to the broadcast using an initializer *)
  let deploy = Connection.lookup  ("deploy" ^ string_of_int ip) ip 
  in  spawn deploy(adv,pc)	

