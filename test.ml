(* let s =  *)
(* let obj =  *)
(* object (self) *)
(*   method foo x = x + 1  *)
(* end in Marshal.to_string obj [Marshal.Closures] *)


(* let () = let o = (Marshal.from_string s 0 : <foo : int -> int; ..>) in print_int (o#foo 3) *)

class dict =
  let foo = def create(n) = print_int n; print_string "\n";  flush stdout;  0
  in create in
object
  method foo = (foo : int Join.chan)
end

let () =
  let mydict = new dict in
  let ns = Connection.ns_server 12345 in
  Join.Ns.register ns "foo" mydict;  while true do () done

(* let () = *)
(*   let foo = def create(n) = print_int n; print_string "\n";  flush stdout;  0  *)
(*   in create in *)
(*   let ns = Connection.ns_server 12345 in *)
(*   Join.Ns.register ns "foo" foo;  while true do () done *)
	
