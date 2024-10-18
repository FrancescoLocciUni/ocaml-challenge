Random.self_init();;

let seven_eleven_doubles () = 
  let d1 = Random.int(6) + 1 in
  let d2 = Random.int(6) + 1 in
  let x = (d1 + d2 == 7) || (d1 + d2 == 11) in
  (x, d1, d2);;


  
