
let knife l n =
  if List.length l <= n then (l, [])
  else if n <= 0 then ([], l)
  else
  let rec aux ls c = match ls with
    | [] -> ([],[])
    | h::t when List.length l - n = List.length t -> (c@[h], t)
    | h::t -> aux t (c@[h]) in
  aux l []
;;


assert (knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));;
assert (knife ['b';'r';'e';'a';'d'] 3  = (['b';'r';'e'], ['a';'d']));;
assert (knife [] 0 = ([], []));;
assert (knife ["miss"; "me"] 2  = (["miss"; "me"], []));;
assert (knife ["oops"] (-1)  = ([], ["oops"]));;