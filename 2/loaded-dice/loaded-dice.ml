
let dice (n:int) =
  (100 - n) / 5;;

let n = Random.int(100) + 1;;
dice(n);;