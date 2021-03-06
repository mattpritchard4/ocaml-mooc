(* TIME ON PLANET SHADOKUS  (30 points possible)
 * On planet Shadokus, a year has 5 months, each month has 4 days, each day has 3 hours and each hour has 2 minutes. A calendar date is therefore defined as the record type date of the given prelude.
 *
 *1.  A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and its minute index is >= 0 and <= 1.
 * The start of year 12 would be:
 * { year = 12; month = 1; day = 1; hour = 0; minute = 0 }
 * The end of year 12 would be:
 * { year = 12; month = 5; day = 4; hour = 2; minute = 1 }
 *
 * Write a function wellformed : date -> bool which checks that the input date is well formed.
 *2. On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a magical computer that evaluates the infinite lambda-term of time. It is defined by value the_origin_of_time of the given prelude.
 * Write a function next : date -> date which computes the date which comes one minute after the input date.
 *3.  In this computer, the time is represented by an integer that counts the number of minutes since 1/1/1 0:0 (the origin of time).
 * Write a function of_int : int -> date that converts such an integer into a date.
 * THE GIVEN PRELUDE
 * type date =
 *   { year : int; month : int; day : int;
 *     hour : int; minute : int }
 *
 * let the_origin_of_time =
 *   { year = 1; month = 1; day = 1;
 *     hour = 0; minute = 0 } *)

type date =
  { year : int; month : int; day : int;
    hour : int; minute : int };;

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 };;

let wellformed date : bool =
  if date.year >= 1 &&
     date.month >= 1 && date.month <= 5 &&
     date.day >=1 && date.day <= 4 &&
     date.hour >= 0 && date.hour <= 2 &&
     date.minute >= 0 && date.minute <= 1
  then
    true
  else
    false;;



let date_to_min date : int =
  let minutes =
    date.minute + (date.hour * 2) in
  if date.year = 1 && date.month = 1 && date.day = 1
  then
    minutes
  else
    let year = (date.year - 1) * 5 * 4 * 3 * 2 in
    let month = (date.month - 1) * 4 * 3 * 2 in
    let day = (date.day - 1) * 3 * 2 in
    year + month + day + minutes
;;

let min_to_date min : date =
  { year = (min / 120) + 1;
    month = ((min / 24) mod 5) + 1;
    day = ((min / 6) mod 4) + 1;
    hour = (min / 2) mod 3;
    minute = min mod 2;
  }
;;

let next date : date =
  min_to_date ((date_to_min date) + 1)
;;
