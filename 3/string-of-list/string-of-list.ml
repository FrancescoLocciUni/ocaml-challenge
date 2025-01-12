(* 
let rec transform_content (l:int list) : string =
  match l with
  | [] -> ""
  | h :: t -> ((string_of_int h) ^ 
  (if t <> [] then ";" else "")
  ^ transform_content t)
;;

let string_of_list (l:int list) : string =
  "[" ^ (transform_content l) ^ "]"
;; *)


let rec content_to_list = function
  | [] -> ""
  | h::t -> (string_of_int h) ^ ( if t <> [] then ";" else "") ^ content_to_list t
;;


let string_of_list (l:int list) =
  "[" ^ content_to_list l ^ "]"
;;











