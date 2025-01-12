

let bounce n =
  let f x =
    if (x mod (2 * n)) <= n then (x mod (2 * n))
    else (n - (x - n) mod (2 * n))
  in f;;

let () =
  let bounce_fn = bounce 5 in
  Printf.printf "bounce(5) 0 = %d\n" (bounce_fn 0);
  Printf.printf "bounce(5) 1 = %d\n" (bounce_fn 1);
  Printf.printf "bounce(5) 2 = %d\n" (bounce_fn 2);
  Printf.printf "bounce(5) 3 = %d\n" (bounce_fn 3);
  Printf.printf "bounce(5) 4 = %d\n" (bounce_fn 4);
  Printf.printf "bounce(5) 5 = %d\n" (bounce_fn 5);
  Printf.printf "bounce(5) 6 = %d\n" (bounce_fn 6);
  Printf.printf "bounce(5) 7 = %d\n" (bounce_fn 7);
  Printf.printf "bounce(5) 8 = %d\n" (bounce_fn 8);
  Printf.printf "bounce(5) 9 = %d\n" (bounce_fn 9);;
