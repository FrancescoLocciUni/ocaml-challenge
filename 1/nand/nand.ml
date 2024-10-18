let nand1 x y = if x && y then false else true;;

let nand2 x y = not(x && y)

let nand3 x y = match(x, y) with
  | (true, true) -> false
  | _ -> true;;