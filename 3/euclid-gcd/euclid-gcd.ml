
let rec gcd (a:int) (b:int) =
  if (a mod b) = 0 then b
  else
    gcd b (a mod b)
;;
