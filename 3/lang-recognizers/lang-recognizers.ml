

(* L0 *)

(* type label_0 = int;;
type state_0 = bool;;

let step_0 (a:label_0) (q:state_0) = match a with
    | 1 -> true
    | _ -> false
;;

let rec memL0_h (q:state_0) (w:label_0 list) = match w with
    | [] -> false
    | [h] -> step_0 h q
    | h::t -> let q' = step_0 h q in memL0_h q' t 
;;

let isInL0 (w:label_0 list) = memL0_h false w;; *)


(* L1 *)

(* type label_1 = int;;
type state_1 = C of bool | E;; (* C( 0 gia trovato), E = errore *)
(* stato finale: (1, t) *)

let step_1 (a:label_1) (q:state_1) = match (a, q) with
    | (0, C true) -> E
    | (0, C false) -> C true
    | (1, q) -> q
    | _ -> failwith "errore, lettera non consentita"
;;

let rec memL1_h (q:state_1) (w:label_1 list) = match w with
    | [] -> q
    | h::t -> let q' = step_1 h q in memL1_h q' t 
;;

let isInL1 (w:label_1 list) = memL1_h (C false) w;; *)


(* L2 *)

(* type label_2 = int;;
type state_2 = C of bool | E;;

let step_2 (a:label_2) (q:state_2) = match (a, q) with
    | (1, C true) -> E
    | (1, C false) -> C true
    | (x, q) when x = 2 || x = 0 -> q
    | _ -> failwith "errore, lettera non consentita"
;;

let rec memL2_h (q:state_2) (w:label_2 list) = match w with
    | [] -> q
    | h::t -> let q' = step_2 h q in memL2_h q' t 
;;


let isInL2 (w:label_2 list) = memL2_h (C false) w;; *)


(* L3 *)

(* type label_3 = int;;
type state_3 = C of bool * int | E;;

let step_3 (a:label_3) (q:state_3) = match (q, a) with
    | (E, _) -> E
    | (C (false, 0), 1) -> q
    | (C (false, 0), 0) -> C (true, 0)
    | (C (true, 0), 0) -> E
    | (C (true, 0), 1) -> C (true, 1)
    | (C (true, 1), 1) -> C (true, 2)
    | (C (true, 1), 0) -> E
    | (C (true, 2), _) -> q
    | _ -> failwith "errore, lettera non consentita"
;;

let rec memL3_h (q:state_3) (w:label_3 list) = match w with
    | [] -> q
    | x::w' -> let q' = step_3 x q in memL3_h q' w'
;;

let isInL3 (w:label_3 list) : state_3 = 
    (memL3_h (C (false, 0)) w) = C (false, 0) ||
    (memL3_h (C (false, 0)) w) = C (true, 2) ;; *)


(* L4 *)

(* type label_4 = int;;
type state_4 = C of int * int | E;;

let step_4 (a:label_4) (q:state_4) = match (q, a) with
    | (C(x,y), 0) -> C(x+1, y)
    | (C(x,y), 1) -> C(x, y+1)
    | (C(x,y), _) -> E
;;


let rec memL4_h (q:state_4) (w:label_4 list) = match w with
    | [] -> q
    | a::w' -> let q' = step_4 a q in memL4_h q' w'
;;

let isInL4 (w:label_4 list) : bool = match memL4_h (C (0, 0) ) w with
    | C (a, b) when a >= b -> true
    | _ -> false
;; *)

(* L4 *)

(* type label_5 = int;;
type state_5 = C of int * int | E;;

let step_5 (a:label_5) (q:state_5) = match (q, a) with
    | (C(x,y), 0) -> C(x+1, y)
    | (C(x,y), 1) -> C(x, y+1)
    | (C(x,y), _) -> E
;;


let rec memL5_h (q:state_5) (w:label_5 list) = match w with
    | [] -> q
    | a::w' -> let q' = step_5 a q in memL5_h q' w'
;;

let isInL5 (w:label_5 list) : bool = match memL5_h (C (0, 0) ) w with
    | C (a, b) when a = b -> true
    | _ -> false
;; *)



(* L6 *)
(* 
type label_6 = int;;
type state_6 = C of int * bool | E;;

let step_6 (a:label_6) (q:state_6) = match (q, a) with
    | (C(0, true), 1) -> E
    | (C(0, false), 0) -> C(1, false)
    | (C(n, false), 0) -> C(n + 1, false)
    | (C(n, false), 1) -> C(n - 1, true)
    | (C(n, true), 1) -> C(n - 1, true)
    | (C(n, true), 0) -> E
    | _ -> E
;;

let rec memL6_h (q:state_6) (w:label_6 list) = match w with
    | [] -> q
    | a::w' -> let q' = step_6 a q in memL6_h q' w'
;;

let isInL6 (w:label_6 list) : bool = match memL6_h (C (0, false) ) w with
    | C (0, true) -> true
    | _ -> false
;; *)



(* L7 *)

(* type label_7 = int;;
type state_7 = C of int * bool | E;;

let step_7 (a:label_7) (q:state_7) = match (q, a) with
    | (C(0, false), 1) -> C(0, true)
    | (C(0, true), _) -> E
    | (C(n, false), 1) -> C(n, true)
    | (C(n, false), 0) -> C(n+1, false)
    | (C(n, true), 0) -> C(n-1, true)
    | _ -> E
;;

let rec memL7_h (q:state_7) (w:label_7 list) = match w with
    | [] -> q
    | a::w' -> let q' = step_7 a q in memL7_h q' w'
;;

let isInL7 (w:label_7 list) : bool = match memL7_h (C (0, false) ) w with
    | C (0, true) -> true
    | _ -> false
;; *)




(* L8 *)


(* type label_8 = int;;
type state_8 = C of int * bool | E;;


let step_8 (a:label_8) (q:state_8) = match (q, a) with
    | (C(0, false), n) when n = 0 || n = 2 -> q
    | (C(0, false), 1) -> C(0,true)
    | (C(n, true), 1) -> C(n,true)
    | (C(n, true), 0) -> C(n+1,true)
    | (C(n, true), 2) -> C(n-1,true)
    | _ -> E
;;

let rec memL8_h (q:state_8) (w:label_8 list) = match w with
    | [] -> q
    | a::w' -> let q' = step_8 a q in memL8_h q' w'
;;

let isInL8 (w:label_8 list) : bool = match memL8_h (C (0, false) ) w with
    | C (0, true) -> true
    | C (0, false) -> true
    | _ -> false
;; *)




(* L9 (equivalente a L6 credo) *)
