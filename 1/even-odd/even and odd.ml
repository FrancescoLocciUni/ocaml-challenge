
let is_even x = x mod 2 == 0;;

let a = Random.int(5) + 1;;
let b = Random.int(5) + 1;;

let win a b = 
  if a >= 1 && a <= 5 && is_even(a + b) then 1
  else
    if a >= 1 && a <= 5 && not is_even(a + b)
      then -1
    else 0;;

win(a, b);;