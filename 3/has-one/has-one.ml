

(* let rec has_one (n:int) : bool = 
  if n > 0 then
    if n mod 10 = 1 then true
    else
      has_one (n / 10)
  else 
    if n = 0 then false
  else failwith("errore, numero non naturale");;  *)


let rec to_list (n:int) :int list =
  if n == 0 then [] else
  n mod 10 :: to_list (n/10) 
;;

let rec has_one (n:int) =
  if n < 0 then failwith "errore, numero negativo";
  List.mem 1 (to_list n)
;;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;