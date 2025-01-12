
let (<|>) a b = match (a,b) with
  | (Some x, None) -> a
  | (None , Some y) -> b
  | (Some x, Some y) -> a
  | _ -> None
;;

assert (Some true <|> Some false = Some true);;
assert (None <|> Some false = Some false);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
assert (None <|> Some "dog" = Some "dog");;
