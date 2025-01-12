
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


let rec merge (l1:int list) (l2:int list) : int list = 
  match (l1, l2) with
  | ([], []) -> [] (* Entrambe le liste sono vuote *)
  | ([], x) -> x (* La prima lista è vuota, restituisci la seconda lista *)
  | (x, []) -> x (* La seconda lista è vuota, restituisci la prima lista *)
  | (h1::t1, h2::t2) when h1 <= h2 -> 
      h1 :: (merge t1 l2) (* L'elemento di testa di l1 è minore o uguale a h2 *)
  | (h1::t1, h2::t2) -> 
      h2 :: (merge l1 t2) (* L'elemento di testa di l2 è minore di h1 *)
;;


assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);;
assert (merge [7] [2;3;6] = [2;3;6;7]);;
assert (merge [7] [] = [7]);;

let halve (l:int list) :(int list * int list) =
  let len = List.length l in
  knife l (len / 2)
;;

assert (halve [1;3;5;8;-2;6] = ([1;3;5], [8;-2;6]));;
assert (halve [1;3] = ([1], [3]));;
assert (halve [1;3;5] = ([1], [3;5]));;


let rec merge_sort (l:int list) :int list = match l with
  | [] -> []
  | [x] -> [x]
  | h::t -> 
    let (left, right) = halve l in
    merge (merge_sort left) (merge_sort right)
;;

assert (merge_sort [1;3;5;8;-2;6] = [-2;1;3;5;6;8]);;