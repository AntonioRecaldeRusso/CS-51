open Core.Std

(* The type Ordering.t from the Core library is used for comparison operations *)
(* type Ordering.t = Less | Equal | Greater ;; *)

let string_compare x y =
  let i = String.compare x y in
    if i = 0 then Equal else if i < 0 then Less else Greater ;;

let int_compare x y =
  let i = x - y in
    if i = 0 then Equal else if i < 0 then Less else Greater ;;
