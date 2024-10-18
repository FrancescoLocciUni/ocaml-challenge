
type weekday = Mo | Tu | We | Th | Fr;;
type course = ALF | LIP;;

let isLecture (d : weekday) (c : course) = 
  (c == ALF && (d == Tu || d == Th || d == Fr)) ||
  (c == LIP && (d == We || d == Th));;


