
let parrot_trouble it h = 
  if h > 7 && h < 20 then None
  else
    if it then Some true else Some false
  ;;


