

let rec to_list (a:int) (b:int) = match a with
  | x when x = b -> [b]
  | x -> x::to_list (a+1) b
;;

let rec min = function
  | [] -> None
  | h::t -> match min t with
              | None -> Some h
              | Some m -> if m < h then Some m else Some h
;;

let rec max = function
  | [] -> None
  | h::t -> match max t with
              | None -> Some h
              | Some m -> if m < h then Some h else Some m
;;


let minmax f (a:int) (b:int) = 
  let range_f = List.map f (to_list a b)
  in
  (min range_f, max range_f)
;;

