
let rec enum_int (n:int) = match n with
  | 0 -> 0
  | x when x mod 2 = 0 -> n / 2
  | _ -> -(n/2 + 1)
;;

assert (List.init 10 enum_int = [0; -1; 1; -2; 2; -3; 3; -4; 4; -5]);;