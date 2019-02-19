(* Write a function move : point -> dpoint -> point such that move p dp is the point p whose coordinates have been updated according to dp.
 * (x is now x +. dx, y is now y +. dy, z is now z +. dz. *)

(* THE GIVEN PRELUDE *)
type point  = { x : float; y : float; z : float };;
type dpoint = { dx : float; dy : float; dz : float };;
type physical_object = { position : point; velocity : dpoint };;

let move p dp : point =
  { x = p.x +. dp.dx;
    y = p.y +. dp.dy;
    z = p.z +. dp.dz
  };;

let first = { x = 1.0; y = 2.0; z = 3.0 };;
let second = { dx = 1.0; dy = 2.0; dz = 3.0 };;

move first second;;

let next obj : physical_object =
  { position = move obj.position obj.velocity; velocity = obj.velocity };;

let will_collide_soon p1 p2 : bool =
  let p3 = next p1 in
  let p4 = next p2 in
  if p3.position.x -. p4.position.x <= 2.0 &&
     p3.position.y -. p4.position.y <= 2.0 &&
     p3.position.z -. p4.position.z <= 2.0 &&
     p3.position.x -. p4.position.x > -2.0 &&
     p3.position.y -. p4.position.y > -2.0 &&
     p3.position.z -. p4.position.z > -2.0
  then
    true
  else
    false;;

