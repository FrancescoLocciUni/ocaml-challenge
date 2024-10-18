
let not_in_range x =
    x < 1 || x > 5
;;

let movie_rating a b c = 
  if (not_in_range a || not_in_range b || not_in_range c)
  then raise (Failure "range non rispettato")
  else
  if (a+b+c = 15) then "Masterpiece"
  else
  if (a+b+c = 14) then "Highly Recommended"
  else
  if (a+b+c >= 11) then "Highly Recommended"
  else
    "Mixed Reviews"
;;

