
let always_fail x = failwith(x);;

let max_nat (a : int) (b : int) : int = 
  if a < 0 || b < 0
    then always_fail "uno o entrabi i numeri non sono naturali"
  else
    if a > b then a else b;;
