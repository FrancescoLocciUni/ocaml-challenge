
(* let sumrange a b = 
  let sum = ref 0 in
  for i = a to b do
    sum := !sum + i
  done;
  !sum
;; *)

let rec to_list (a:int) (b:int) = match a with
  | x when x = b -> [x]
  | x -> x::(to_list (x+1) b)
;;


let sumrange (a:int) (b:int) :int =
  if a > b then 0 else
  (to_list a b)
  |> List.fold_left (+) 0
;;

assert (sumrange 0 1 = 1);;

assert (sumrange 1 3 = 6);;

assert (sumrange 3 2 = 0);;

assert (sumrange 3 3 = 3);;