
let guess5 n =
  if n < 1 || n > 5 then failwith "Number out of range"
  else
    let r = Random.int(n) + 1 in
    (n == r, r)
;;