
type student = 
{
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let alf2024 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;


let rec id_of_noshow = function
  | [] -> []
  | h::t when h.vote = None -> h.id :: id_of_noshow t
  | _::t -> id_of_noshow t
;;


let rec upgradeable = function
  | [] -> []
  | h::t when h.vote >= Some 15 && h.vote <= Some 17 -> 
    ( h.name ^ " " ^ h.surname ):: upgradeable t
  | _::t -> upgradeable t
;;




let upgrade students =
  students |> List.map(
    fun s -> match s.vote with
    | Some v when v >= 15 && v <= 17 -> { s with vote = Some 18 }
    | _ -> s
  )
;;

let wrong_laude (l:student list) =
  l 
  |> List.filter( fun s -> s.laude && ( s.vote = None || s.vote < Some 30) )
  |> List.map( fun s -> s.name ^ " " ^ s.surname )
;;

let fix_laude (l:student list) =
  l
  |> List.map
  (
    fun s -> match s.vote with
    | None -> { s with laude = false }
    | Some v when v < 30 -> { s with laude = false }
    | _ -> s
  )
;;



let percent_passed (l:student list) =
  if l = [] then 0.0 else
  let n_passed = l
  |> List.filter(fun s -> s.vote >= Some 18)
  |> List.length in
  float_of_int(n_passed) /. float_of_int(l |> List.length)
;;


let avg_vote (l:student list) =
  let votes = 
    l 
    |> List.filter (fun s -> s.vote <> None)
    |> List.map (fun s -> match s.vote with Some v -> v | None -> 0)
  in
  let sum = List.fold_left (+) 0 votes in
  let count = List.length votes in
  if count = 0 then 0 else sum / count
;;