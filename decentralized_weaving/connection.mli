val addr : int -> Unix.sockaddr
val server : int -> Join.Site.t
val ns : int -> Join.Ns.t
val lookup : string -> int -> 'a
val ns_server : int -> Join.Ns.t
