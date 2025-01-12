
type suit = S | H | D | C;;
type card = Card of int * suit;;

let are_in_order((a:card),(b:card),(c:card),(d:card),(e:card)) : bool = match(a,b,c,d,e) with
  | (Card(va,sa),Card(vb,sb),Card(vc,sc),Card(vd,sd),Card(ve,se)) when va <= vb && vb <= vc && vc <= vd && vd <= ve -> true
  | _ -> false
;;

let straight((a:card),(b:card),(c:card),(d:card),(e:card)) : bool = are_in_order(a,b,c,d,e);;


let rndCard() = 
  let v = Random.int(13) + 1 in
  let s = match Random.int(4) with
  | 0 -> S
  | 1 -> H
  | 2 -> D
  | 3 -> C in
  Card(v,s);;



let rndHand = (rndCard(),rndCard(),rndCard(),rndCard(),rndCard());;

let c1, c2, c3, c4, c5 = rndHand;;
straight(c1,c2,c3,c4,c5);;