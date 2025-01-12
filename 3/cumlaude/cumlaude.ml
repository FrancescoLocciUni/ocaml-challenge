
type grade = Val of int | CumLaude;;

let int_of_grade = function
  | Val x when x >= 1 && x <= 30 -> x
  | CumLaude -> 32
  | _ -> failwith "valore di voto non valido"
;;

let is_valide (x:grade) :bool =
  let grade = int_of_grade x in 
  (grade >= 18 && grade <= 30 || grade = (int_of_grade CumLaude))
;;

let avg (l:grade list) =
  (l 
  |> List.map (fun x -> int_of_grade x)
  |> (List.fold_left (+) 0 ) ) / (List.length l)
;;
