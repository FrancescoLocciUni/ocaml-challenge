

let consecutive_even (l:int list) = 
  let rec aux l max_l, curr_l = match l with
    | [] -> max max_l curr_l
    | h::t -> if h mod 2 = 0 then
        aux t max_l (curr_l + 1)
      else
        aux t (max_l curr_l) 0
      int aux l 0 0
;;


assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);; 
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;