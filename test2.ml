(* let s =  *)
(* let obj =  *)
(* object (self) *)
(*   method foo x = x + 1  *)
(* end in Marshal.to_string obj [Marshal.Closures] *)


(* let () = let o = (Marshal.from_string s 0 : <foo : int -> int; ..>) in print_int (o#foo 3) *)

let () = let dict = Connection.lookup "foo" 12345 in spawn (dict#foo(5));  while true do () done
