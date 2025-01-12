
(* let countzero f a b = 
  let count = ref 0 in
  for i = a to b do
    if f i = 0 then incr count
  done;
  !count
;; *)


let rec to_list (a:int) (b:int) = match a with
  | x when x = b -> [b]
  | x -> x::to_list (a+1) b
;;

let rec map f = function
  | [] -> []
  | h::t -> f h :: map f t
;;

(* let countzero f (a:int) (b:int) = 
  let f_range = map f (to_list a b) in
  let rec count = function 
    | [] -> 0
    | h::t when h = 0 -> 1 + count t
    | _::t -> 0 + count t
  in
  count f_range 
;; *)


let countzero f (a:int) (b:int) = 
  (map f (to_list a b))
  |> (map (fun x -> if x == 0 then 1 else 0) )
  |> List.fold_left (+) 0
;;




assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;