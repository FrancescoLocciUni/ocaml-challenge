let mux1 s0 a b = if s0 then a else b;;
let mux1_1 s0 a b = s0 && a || (not s0) && b;;

let mux2 s0 a b = match(s0) with
  | (true) -> a
  | _ -> b;;

let mux4 s1 s0 a0 a1 a2 a3 = match(s1, s0) with
  | (true, true) -> a3
  | (true, false) -> a2
  | (false, true) -> a1
  | (false, false) -> a0;;

let mux4 s1 s0 a0 a1 a2 a3 =
  let m0 = mux2 s0 a0 a1 in
  let m1 = mux2 s0 a2 a3 in
  mux2 s1 m0 m1;;

let mux4_1 s1 s0 a0 a1 a2 a3 =
  mux2 s0
    (* s0 = 1 *)
    (mux2 s1
      (* s1 = 1 *) a3 
      (* s1 = 0 *) a2)
    (* s0 = 0 *)
    (mux2 s1
      (* s1 = 1 *) a1 
      (* s1 = 0 *) a0)

      
     
