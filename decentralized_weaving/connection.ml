(* A distant server can be given as comand-line argument *) 
let addr n_addr = 
  let addr' =
    (* if Array.length Sys.argv > 1 then *)
    (*   (Unix.gethostbyname Sys.argv.(1)).Unix.h_addr_list.(0) *)
    (* else *)
    Join.Site.get_local_addr () 
  in Unix.ADDR_INET (addr',n_addr)
  
(* Die when server dies *)
let server n_addr = 
  let addr = addr n_addr in
  let server' = Join.Site.there addr in
  let () =  Join.Site.at_fail server' (def bye() = exit 0 ; 0 in bye)
  in server'
  
(* Get register channel *)
let ns n_addr = Join.Ns.of_site (server n_addr)

(* Get channel registered at 'key *)  
let rec lookup key n_addr =
  try (Join.Ns.lookup (ns n_addr) key) ; 
  with Not_found -> Thread.delay 1.0 ; lookup key n_addr

(* generate a distant server *)

let ns_server n_addr = 
  let addr = addr n_addr in
  let () =  Join.Site.listen addr in
  Join.Ns.there addr
