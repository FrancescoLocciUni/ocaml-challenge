
let f0 (x:int) = x;;
let f1 (y:int) = y+4;;
let f2 (z:int) = 5/z;;

let consensus3 (f0, f1, f2) n = 
  match (f0 n, f1 n, f2 n) with
  | (None, _, _) | (_, None, _) | (_, _, None) -> None
  | (Some v0, Some v1, Some v2) -> 
    if (v0 = 5 && v1 = 5) || (v1 = 5 && v2 = 5) || (v0 = 5 && v2 = 5) then Some 5
    else None;;
