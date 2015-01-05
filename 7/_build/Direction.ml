open Core.Std
(** Enumeration type of directions. *)
type direction =
  | North | South | East | West
  | NorthEast | SouthEast | NorthWest | SouthWest

(** A list containing all directions. *)
let all : direction list =
  [ North; South; East; West; NorthEast; SouthEast; NorthWest; SouthWest ]

(** Generate a direction from an integer n. It is expected 0 <= n < 8 *)
let ord (n:int) : direction =
  match n with
    | 0 -> North | 1 -> South | 2 -> East | 3 -> West
    | 4 -> NorthEast | 5 -> SouthEast | 6 -> NorthWest | _ -> SouthWest

(** Generate a random direction given a random number generator. *)
let random (r:int -> int) : direction = ord (r 8)

(** Convert a direction to a change in the x and y coordinates. *)
let to_vector (d:direction) : int*int =
  match d with
    | North -> (0,1) | South -> (0,-1)
    | East -> (1,0) | West -> (-1,0)
    | NorthEast -> (1,1) | SouthEast -> (1,-1)
    | NorthWest -> (-1,1) | SouthWest -> (-1,-1)

(** The distance between two points. *)
let distance ((x1,y1):int*int) ((x2,y2):int*int) : float =
  sqrt (Float.of_int (x1 - x2) ** 2. +. Float.of_int (y1 - y2) ** 2.)

(** Move a point in the specified direction. *)
let move_point ((x,y):int*int) (dM:direction option) : int*int =
  match dM with
  | None -> (x,y)
  | Some d ->
      let (dx, dy) = to_vector d in
      (x+dx, y+dy)

(** The natural direction between two points is the diagonal direction between
    them if two two points are not directly east-west or north-south, and the
    non-diagonal direction between them otherwise. *)
let natural ((x1,y1):int*int) ((x2,y2):int*int) : direction option =
       if x1 < x2 && y1 < y2 then Some NorthEast
  else if x1 < x2 && y1 = y2 then Some East
  else if x1 < x2 && y1 > y2 then Some SouthEast
  else if x1 = x2 && y1 < y2 then Some North
  else if x1 = x2 && y1 = y2 then None
  else if x1 = x2 && y1 > y2 then Some South
  else if x1 > x2 && y1 < y2 then Some NorthWest
  else if x1 > x2 && y1 = y2 then Some West
  else if x1 > x2 && y1 > y2 then Some SouthWest
  else failwith "impossible"

(** The natural path is the list of points between p1 and p2 following the
    natural direction *)
let rec natural_path (p1:int*int) (p2:int*int) : (int*int) list =
    let dM = natural p1 p2 in
    match dM with
    | None -> []
    | Some d ->
        let p1' = move_point p1 (Some d) in
        p1'::natural_path p1' p2
