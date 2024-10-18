
type season = Spring | Summer | Autumn | Winter;;

let squirrel_play t s = match(s, t) with
  | (Summer, _) when (t >= 15 && t <= 35) -> true 
  | (_ , _) when (t >= 15 && t <= 30) -> true
  | _ -> false;;

assert(squirrel_play 18 Winter = true);;
assert(squirrel_play 32 Spring = false);;
assert(squirrel_play 32 Summer = true);;