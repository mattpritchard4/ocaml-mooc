(* SEARCHING FOR STRINGS IN ARRAYS  (30 points possible)
 * Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
 * Using the binary search algorithm, an element can be found very quickly in a sorted array.
 * Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
 * The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable. *)

(* Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare). *)

(* compare array[0] to mapped_array[1]. Tail recurse through each pair
until you hit the boundary. *)

(* let is_sorted a : bool =
 *   let mapped_a = map (fun -> )
 * ;; *)

let id (a : 'a) : 'a = a;;

(* ('a -> 'b) -> (int -> 'a -> 'b) *)

let extend (a: 'a -> 'b) : int -> 'a -> 'b =
  fun (_: int) (answer: 'a) -> a answer
;;

(* to join a new element onto the list, cons the f(element) with the current list, feed this as input back into the function *)

let mymap (f: 'a -> 'b) (l: 'a list) : 'b list =
  let start = 0 in
  let newlist = [] in
  let rec build index list  =
    if index = List.length l
    then
      list
    else
      build (index + 1) ((f (List.nth l index))::list)  in
  List.rev (build start newlist)
;;

let rec mymap (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | hd :: tl -> f hd :: mymap f tl
;;

let is_sorted (a: string array) : bool =
  let answer =
    Array.mapi (fun i x ->
    if i = 0
    then
      true
    else if String.compare x a.(i - 1) > 0
    then
      true
    else
      false) a in
  Array.for_all (fun y -> y) answer
;;
