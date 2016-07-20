type bool3 = True | False | Unknown

let bool3_to_bool b = match b with 
  | True -> true
  | False -> false
  | Unknown -> true

let bool_to_bool3 b = if b then True else False 

let and3 b1 b2 = match (b1,b2) with 
  | (Unknown,Unknown) -> Unknown
  | (True,Unknown) -> Unknown
  | (Unknown,True) -> Unknown
  | (False,Unknown) -> False
  | (Unknown,False) -> False
  | (True,True) -> True
  | (False,True) -> False
  | (False,False) -> False
  | (True,False) -> False

let or3 b1 b2 = match (b1,b2) with 
  | (Unknown,Unknown) -> Unknown
  | (True,Unknown) -> True
  | (Unknown,True) -> True
  | (False,Unknown) -> Unknown
  | (Unknown,False) -> Unknown
  | (True,True) -> True
  | (False,True) -> True
  | (False,False) -> True
  | (True,False) -> False

let not3 b = match b with 
  | True -> False
  | False -> True
  | Unknown -> Unknown
