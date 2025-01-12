

let filtra f k =
  f
  |> List.map (fun x -> if fst x = k then snd x else 0)
  |> List.filter (fun x -> x <> 0)
;;

let apply f k =
  let filtrati = filtra f k in
  match filtrati with
  | [] -> None
  | h::_ -> Some h
;;


let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)];;

assert(apply f0 4 = Some 5);;
assert(apply f0 6 = None);;
assert(apply f0 2 = Some 3);;


(* 
let filtra2 k f =
  f
  |> List.map (fun x -> if fst x <> k then x else (0,0))
  |> List.filter (fun x -> x <> (0,0))
;;

let mkfun f =
  f
  |> List.map (fun x -> 
    match f with
    | h::t -> filtra2 (fst x) t)
;; *)

let mkfun f =
  List.fold_right (fun (k, v) acc ->
    let keys = List.map fst acc in
    if List.mem k keys then acc else (k, v) :: acc
  ) f []
;;






assert(mkfun [(1,7);(2,3)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(1,3)] = [(1,7)]);;
assert(mkfun [(1,7);(2,3);(1,5)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8);(2,4)] = [(1,7);(2,3)]);;