
let rec rev = function
  | [] -> []
  | h::t -> rev t @ [h]
;;


let is_palindrome l =
  if l = [] then true else
  let rec aux l1 l2 = match (l1, l2) with
    | ([],[]) -> true
    | (h1::t1, h2::t2) when h1 <> h2 -> false
    | (h1::t1, h2::t2) -> aux t1 t2
    | _ -> false
  in
  aux l (rev l) 
;;

assert(is_palindrome []);;
assert(is_palindrome ['a';'n';'n';'a']);;
assert(is_palindrome ['r';'a';'d';'a';'r']);;
assert(is_palindrome ['a';'n';'n';'e'] = false);;
assert(is_palindrome ['z';'a';'n';'n';'a'] = false);;