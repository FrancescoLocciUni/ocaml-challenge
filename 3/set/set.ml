
let rec mem x = function
  | h::t when h = x -> true
  | _::t when t <> [] -> mem x t
  | _ -> false 
;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;


let rec subseteq xl yl = match xl with
  | [] -> true
  | h::t -> if mem h yl then subseteq t yl else false
;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;


let rec seteq l1 l2 = match (l1,l2) with
  | ([], []) -> true
  | (h1::t1, h2::t2) when mem h1 l2 && t1 <> [] -> seteq t1 l2
  | (h1::t1, h2::t2) when mem h1 l2 && t1 = [] -> true
  | _ -> false
;;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;


let rec dup l1 = match l1 with
  | [] -> false
  | h::t when t <> [] -> mem h t || dup t
  | h::t when t = [] -> false
;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;


let rec mkset = function
  | [] -> []
  | h::t when not (mem h t) -> h :: mkset t
  | h::t when mem h t -> mkset t
;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;


let union l1 l2 = mkset (l1 @ l2);;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;


let rec inter l1 l2 = match (l1, l2) with
  | ([], _) -> []
  | (_, []) -> []
  | (h::t, _) when mem h l2 -> h :: inter t l2
  | (h::t, _) when not (mem h l2) -> inter t l2
;;

assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;


let rec diff l1 l2 = match (l1, l2) with
  | ([], _) -> []
  | (x, []) -> x
  | (h::t, _) when mem h (inter l1 l2) -> diff t l2
  | (h::t, _) when not (mem h (inter l1 l2)) -> h :: diff t l2
;;


assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;


let rec dsum xl yl = match (xl, yl) with
  | ([],[]) -> []
  | (h::t, []) -> (0, h) :: dsum t yl
  | ([], h::t) -> (1, h) :: dsum xl t
  | (h1::t1, h2::t2) -> (0, h1) :: dsum t1 yl
;;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;


let rec powset xl = match xl with
  | [] -> [[]]
  | h::t ->
    let ps = powset t in
    ps @ List.map (fun subset -> h :: subset) ps
;;

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;