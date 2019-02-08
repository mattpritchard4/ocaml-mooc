(* INTEGER IDENTIFIERS
 * Suppose that a variable x exists and is an integer.
 *
 * Define a variable x_power_8 that uses three multiplications to calculate x to the power of 8. The only function you are allowed to call is the (\*\) operator. *)

let x_power_8 x: int =
  let y = x * x in
  let z = y * y in
  z * z;;
