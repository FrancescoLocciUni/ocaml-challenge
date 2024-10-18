
let max a b c =
  if a > b && a > c then a
  else
    if b > c then b else c
;;

let best_offer a b c = match (a, b, c) with
  | (None,None,None) -> None
  | _ -> Some(max a b c)
;;


