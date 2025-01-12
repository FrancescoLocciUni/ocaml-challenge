
type bitstring = E | Z of bitstring | U of bitstring;;
(* E = empty string, Z s = stringa di un 0 seguita da s, U = stringa di un 1
seguita da s 

  E   = ""
  Z s = "0(s...)"
  U s = "1(s...)"

  Z( Z( U( U(E) ) ) ) = "0011"
*)

let rec string_of_bitstring = function
  | E -> ""
  | Z s -> "0" ^ string_of_bitstring s
  | U s -> "1" ^ string_of_bitstring s
;;

let rec len = function
  | E -> 0
  | Z s -> 1 + len s
  | U s -> 1 + len s
;;

let rec countZ = function
  | E -> 0
  | Z s -> 1 + countZ s
  | U s -> 0 + countZ s
;;


let rec countU = function
  | E -> 0
  | Z s -> 0 + countU s
  | U s -> 1 + countU s
;;

(* Z(U(E)) ^ U(U(Z(E))) --> Z(U(U(U(Z(E))))) *)
(*    01   ^    110     -->      01_110 *)
let rec concat (a:bitstring) (b:bitstring) = match a with
  | E -> b
  | U x -> U (concat x b)
  | Z x -> Z (concat x b)
;;


let rec equals (a:bitstring) (b:bitstring) = match (a,b) with
  | (E,E) -> true
  | (U x, U y) -> equals x y
  | (Z x, Z y) -> equals x y
  | _ -> false
;;


let tl = function
  | E -> E
  | U x -> x
  | Z x -> x
;;

let rec prefix (a:bitstring) (b:bitstring) = match (a,b) with
  | (E,_) -> true
  | (U x, U y) -> prefix x y
  | (Z x, Z y) -> prefix x y
  | _ -> false
;;


let rec substr (a:bitstring) (b:bitstring) = match (a, b) with
  | (E, _) -> true  (* La stringa vuota è sempre una sottostringa *)
  | (_, E) -> false (* Una stringa non vuota non può essere una sottostringa di una stringa vuota *)
  | (Z x, Z y) -> (substr x y)
  | (U x, U y) -> (substr x y)
  | (Z x, U y) -> (substr a y)
  | (U x, Z y) -> (substr a y)
;;

