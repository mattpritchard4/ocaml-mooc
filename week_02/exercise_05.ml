(* SEARCHING FOR STRINGS IN ARRAYS  (30 points possible)
 * Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
 * Using the binary search algorithm, an element can be found very quickly in a sorted array. 
 * Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr. 
 * The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable. *)

let is_sorted a : bool =
  Array.fold_left (fun initial accumulator ->
      let answer = String.compare(accumulator initial) > 0 in
      
;;
