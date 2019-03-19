let is_sorted a : bool =
  let sorted = Array.fold_left (fun initial accumulator ->
      String.compare accumulator initial) a.(0) a in
  if sorted = 0
  then true
  else false
;;
