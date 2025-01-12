
type winner = Player | Computer | Tie;;

let win((hp:int),(gp:int)) = 
  let hc = Random.int(5) in
  let gc = Random.int(5) in
  if gp = hp + hc then ((gp,gc), Player)
  else if gc = hp + hc then ((gp,gc), Computer)
  else ((gp,gc),Tie);;