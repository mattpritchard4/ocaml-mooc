(* FINDING THE MINIMUM  (20 points possible)
 * Consider a non empty array of integers a.
 *
 * Write a function min : int array -> int that returns the minimal element of a.
 * Write a function min_index : int array -> int that returns the index of the minimal element of a.
 * Do you think these functions work well on large arrays ?
 *
 * Define a variable it_scales and set it to "yes" or "no". *)

let min a : int =
  Array.fold_left (fun initial accumulator ->
      if accumulator < initial
      then accumulator
      else initial) a.(0) a
;;

let rec find_index array value index : int =
  if array.(index) = value
  then
    index
  else
    find_index array value (index - 1)
;;

let min_index a : int =
  find_index a (min a) (Array.length a - 1)
;;

let it_scales = "no";;
