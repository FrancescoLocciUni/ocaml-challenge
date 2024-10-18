let xor1 x y = not(x == y);;

let xor2 x y = if x == y then false else true;

let xor3 x y = match(x, y) with
  | (true, true) -> false
  | (false, false) -> false
  | _ -> true;;