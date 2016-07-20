open Weaving_registry

(************************)
(* The distributed loop *)
(************************)

let () = let _ = new weaving_registry 12345 in
	 let _ = new weaving_registry 54321 in
	 while true do Thread.delay 1.0 done
