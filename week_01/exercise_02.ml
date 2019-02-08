(* STRING IDENTIFIERS  (2/2 points)
 * Suppose that a variable word exists and is a string.
 *
 * Define a variable sentence that uses 5 string concatenations to create a string containing 9 times word, separated by commas (',').
 *
 * This time, experiment with defining local let ... ins to store the partial results. *)

let word = "hello";;

let sentence =
  let part1 = word ^ "," in
  let part2 = part1 ^ part1 in
  let part3 = part2 ^ part2 in
  let part4 = part3 ^ part3 in
  part4 ^ word;;
