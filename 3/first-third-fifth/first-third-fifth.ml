
let first_third_fifth l =

  if List.length l < 5 then None
  else
    match l with
    | [a;_;b;_;c;_] -> Some (a,b,c)
;;
