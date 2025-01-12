
type suit = S | H | D | C;;
type card = Card of int * suit;;

let rndCard() = 
  let v = Random.int(13) + 1 in
  let s = match Random.int(4) with
  | 0 -> S
  | 1 -> H
  | 2 -> D
  | 3 -> C in
  Card(v,s);;



let rndHand = (rndCard(),rndCard(),rndCard(),rndCard(),rndCard());;



let is_poker((a:card),(b:card),(c:card),(d:card)) = match(a,b,c,d) with
  | (Card(va,sa),Card(vb,sb),Card(vc,sc),Card(vd,sd)) when va = vb && vb = vc && vc = vd &&
    sa <> sb && sb <> sc && sc <> sd -> true
  | _ -> false
;;

let poker((a:card),(b:card),(c:card),(d:card),(e:card)) = match(a,b,c,d,e) with
  | (_, x, y, z, w) when is_poker(x,y,z,w) -> true
  | (x, _, y, z, w) when is_poker(x,y,z,w) -> true
  | (x, y, _, z, w) when is_poker(x,y,z,w) -> true
  | (x, y, z, _, w) when is_poker(x,y,z,w) -> true
  | (x, y, z, w, _) when is_poker(x,y,z,w) -> true
  | _ -> false
;;

let c1, c2, c3, c4, c5 = rndHand;;
poker(c1,c2,c3,c4,c5);;