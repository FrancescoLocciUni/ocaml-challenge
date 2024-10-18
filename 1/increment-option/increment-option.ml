
let incr_opt value = match value with
    | None -> None
    | Some value -> Some(value + 1);;

incr_opt (Some 5) = Some 6;;
incr_opt None = None;;