
type nat = Z | S of nat;;

let to_int (x:nat) =
    let rec count_f (n:nat) (acc:int) = match n with
    | Z -> acc
    | S k -> count_f k (acc+1)
    in
    count_f x 0;;


let iseven (x:nat) = if x = Z then true else 
    if to_int(x) mod 2 = 0 then true else false
;;


let halve (n:nat) : nat =
    let rec aux x count = match x with
        | Z -> Z
        | S Z -> if count = 0 then Z else S Z
        | S (S v) -> S (aux v count)
        | S v -> aux v (count - 1)
    in
    aux n 0
;;


let add (a:nat) (b:nat) :nat =
    if a = Z then b
    else if b = Z then a
    else
        let rec aux (x:nat) (y:nat) = 
            match x with
            | S Z -> S (y)
            | S (n) -> S ( aux n y )
        in
        aux a b
;;


let mul (a:nat) (b:nat) :nat =
    if a = Z then Z
    else if b = Z then Z
    else
        let rec aux (n:int) (x:nat) (y:nat) = 
            if n = 1 then x
            else
                aux (n-1) (add x y) y
        in
        aux (to_int b) a a
;;


let rec equals (a:nat) (b:nat) = match (a,b) with
    | (Z,Z) -> true
    | (S(x),S(y)) -> equals x y
    | _ -> false
;;

let equals2 (a:nat) (b:nat) = (to_int a) = (to_int b);;


let leq (a:nat) (b:nat) = 
    if equals a b then true
    else
        let rec aux (x:nat) (y:nat) = match (x,y) with
            | (Z, S(_)) -> true
            | (S(x'), S(y')) -> aux x' y'
            | _ -> false
        in
        aux a b
;;


