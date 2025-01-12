
let f1 (a:int) :bool = a > 0;;
let f2 (a:bool) :int = if a then 1 else 0;;
let f3 (a:int) = (a,f1 a);;
let f4 ((a:int),(b:bool)) : int = a;;
let f5 (a:int) = fun x -> x + a;;
let f6 (a:int) = fun x -> (x + a > 0);;
let f7 (a:bool) = fun x -> (x > 0 && a);;
let f8 (a:bool) = fun x -> f2 x;;
let f9 (a:bool) = fun x -> (if a then x + 1 else x);;

let f10 (a:int) (b:int) = a + b;;
let f11 (a:int) (b:bool) = if b then a else a + 1;;
let f12 (a:bool) (b:int) = if a then b else b + 1;;
let f13 (a:int) (b:bool) = a > 0 && b;;
let f14 (a:bool) (b:bool) = (f2 a) + (f2 b);;

let f15 (a:int) ((x:int),(y:int)) = a + x * y;;
let f16 (a:int) = fun x -> (fun y -> a + x * y);;
let f17 (a:int) (b:int) = fun x -> a + x * b;;
let f18 f (c:int) = f (c) + 1;;
let f19 f = fun x -> ( f (f2 x) <> 0 );;
let f20 f = fun x -> ( f2 (f (f2 x) ) );;