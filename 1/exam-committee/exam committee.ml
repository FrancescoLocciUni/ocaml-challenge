
type vote =
  | StrongReject
  | WeakReject
  | WeakAccept
  | StrongAccept;;

let voto_positivo v = match v with
  | WeakAccept | StrongAccept -> true
  | _ -> false;;


let esame_passato (v1, v2, v3) =
  let x = 
    (if voto_positivo v1 then 1 else 0) + 
    (if voto_positivo v2 then 1 else 0) + 
    (if voto_positivo v3 then 1 else 0) 
  in
  if x >= 2 then 
    if v1 = StrongReject || v2 = StrongReject || v3 = StrongReject then false
    else true
  else 
    false

