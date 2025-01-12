
let f1 (x:int) = 0;;

let f2 (x:int) = match x with
  | 1 -> 1
  | 0 -> 1
  | 2 -> 1
  | _ -> 0
;;

let f3 (x:int) = if x > 0 && x < 100 then 1 else 0;;

let f4 (x:int) = 0;;

let f5 (x:int) = 1;;

let f6 (x:int) = if x mod 2 = 0 then 1 else 0;;

let f7 (x:int) = f6 x;;

let f8 (x:int) = if x >= 0 then 1 else failwith("numero non consentito");;

let f9 (x:int) = 1;;

let f10 (x:int) = if x < 20 && x > 0 || x > 4 && x < 10 then 1 else 0;;

let f11 (x:int) = if x mod 2 = 0 && x < 50 then 1 else 0;;

let f12 (z:int) = if z mod 2 = 0 && 0 < z && z < 50 then 1 else 0;;

let f13 (z:int) = 1;;

let f14 (x:int) = if x = 2 then 1 else 0;;

let is_prime (x:int) =
  let rec check_divisors (d:int) =
    if d = 1 then true
    else if x mod d = 0 then false
    else check_divisors (d-1) in
  if x < 2 then false
  else check_divisors (x-1);;

let f15 (x:int) = if is_prime x then 1 else 0;;
