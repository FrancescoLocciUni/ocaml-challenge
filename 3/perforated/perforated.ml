
(* la differenza tra succ e prec deve essere almeno 2 *)

let rec is_perforated (l:int list) = match l with 
  | [] -> true
  | [_] -> true
  | fs::sc::t when (abs (sc - fs) < 2) -> false
  | _::sc::t -> is_perforated (sc::t)
;;


assert(is_perforated []);;
assert(is_perforated [1]);;
assert(is_perforated [1;2] = false);;
assert(is_perforated [1;3]);;
assert(is_perforated [1;5;2]);;
assert(is_perforated [1;3;2] = false);;
assert(is_perforated [1;4;2;0]);;
assert(is_perforated [1;3;2;0] = false);;
assert(is_perforated [1;3;5;2;4;7;3;1]);;