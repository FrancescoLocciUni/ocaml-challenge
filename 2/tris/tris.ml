  

let tris((a : int),(b : int),(c : int),(d : int)) =
  if ( a = b && b = c || a = b && b = d || b = c && c = d) then true
  else false;;

let hand() = 
  let a = Random.int(10) + 1 in
  let b = Random.int(10) + 1 in
  let c = Random.int(10) + 1 in
  let d = Random.int(10) + 1 in
  (a,b,c,d);;

let (a,b,c,d) = hand();;

tris(a,b,c,d);;
