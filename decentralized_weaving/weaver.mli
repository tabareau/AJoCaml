open Join_point

class advisable_weaver :
  reaction_info -> int ->
  object
    (* method add_aspect : *)
    (*   ((control_flow * (control_flow * dynamic_jp) Join.chan * dynamic_jp * dynamic_jp list) *)
    (*    Join.chan *pcd) Join.chan *)
    method weave :
      (control_flow * (control_flow * dynamic_jp) Join.chan * dynamic_jp)
      Join.chan
  end

class observable_weaver :
  reaction_info -> int ->
  object
    (* method add_aspect : ((control_flow * dynamic_jp * dynamic_jp list) Join.chan * pcd) Join.chan *)
    method weave : (control_flow * (control_flow * dynamic_jp) Join.chan * dynamic_jp)
      Join.chan
  end

class ['a] opaque_weaver :
  reaction_info -> int ->
  object
    method weave :
      (control_flow * (control_flow * dynamic_jp) Join.chan * dynamic_jp)
      Join.chan
  end
