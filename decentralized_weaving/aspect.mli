open Join_point

val aspect : int -> pcd ->
  (control_flow * (control_flow * dynamic_jp) Join.chan * dynamic_jp * dynamic_jp list) Join.chan -> unit

val aspect_before : int -> pcd ->
  (control_flow * dynamic_jp * dynamic_jp list) Join.chan -> unit


