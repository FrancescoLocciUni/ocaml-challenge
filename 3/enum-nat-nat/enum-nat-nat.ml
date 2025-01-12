let enum_nat_nat (n:int) =
  let max_dx = ref 0 in
  let sx = ref 0 in
  let dx = ref 0 in

  if n = 0 then (0, 0)
  else begin
    for i = 1 to n do
      if !dx = 0 then begin
        sx := 0;
        dx := !max_dx + 1;
        max_dx := !max_dx + 1
      end else begin
        sx := !sx + 1;
        dx := !dx - 1
      end
    done;
    (!sx, !dx)
  end
;;
