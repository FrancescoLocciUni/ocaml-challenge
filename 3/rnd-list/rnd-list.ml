

let rec rnd_list (n:int) (b:int) :int list =
  if n <= 0 then []
  else Random.int(b) + 1 :: rnd_list (n - 1) b
;;
