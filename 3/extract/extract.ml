let rec get_element i = function
  | a::t when i = 0 -> a
  | _::t -> get_element (i-1) t
  | [] -> failwith "indice fuori dai limiti"
;;

let rec new_list e = function
  | [] -> []
  | h::t when h <> e -> h :: new_list e t
  | _::t -> new_list e t
;;

let extract i l =
  if i < 0 || i > (List.length l) - 1 then
    failwith "errore, indice non compreso"
  else
    let item = get_element i l in
    let nl = new_list item l in
    (item, nl)
;;

