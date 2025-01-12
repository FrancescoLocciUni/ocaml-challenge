let is_posfrac (a, b) = 
  (a /. b) >= 0.;;

let compare_posfrac (a, b) (c, d) = 
  if (not (is_posfrac (a, b)) || not (is_posfrac (c, d))) then 
    failwith "errore, [NOT A FRACTION]"
  else 
    let ab = a /. b in
    let cd = c /. d in
    if ab = cd then 0
    else if ab > cd then 1
    else -1;;