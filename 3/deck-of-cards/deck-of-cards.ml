(* Funzione per generare un numero casuale nell'intervallo [min, max] *)
let random_range min max =
  min + Random.int (max - min + 1)
;;

(* Definizione dei semi e delle carte *)
type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

(* Verifica se una carta è già nel mazzo *)
let rec already_in_deck (l:card list) (c:card) : bool =
  match l with
  | [] -> false
  | h::t when h = c -> true
  | _::t -> already_in_deck t c
;;

(* Verifica se il mazzo è completo *)
let rec is_complete (l:card list) : bool = 
  if List.length l <> 40 then false
  else
    match l with
    | Card(n,_)::t when n < 1 || n > 10 -> false
    | h::t -> (not (already_in_deck t h)) && (is_complete t)
    | [] -> true
;;

(* Generazione del mazzo *)
let gen_deck () = 
  let rec gen_card n l =
    if n = 0 then l
    else
      let c = 
        let s = random_range 0 3 in
        Card(random_range 1 10, match s with 
          | 0 -> Spades 
          | 1 -> Hearts 
          | 2 -> Diamonds 
          | 3 -> Clubs) 
      in
      if already_in_deck l c then gen_card n l
      else gen_card (n - 1) (c :: l)
  in
  gen_card 40 []
;;
