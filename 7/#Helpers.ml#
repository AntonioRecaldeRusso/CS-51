open Core.Std
(** A package of generic and useful helper functions. *)
exception TODO

(*************************)
(***** Array Helpers *****)
(*************************)

(** Modify each element of an array in place *)
let array_map_modify (f:'a -> 'a) (arr:'a array) : unit =
  Array.iteri ~f:(fun i _ -> arr.(i) <- f (arr.(i))) arr

(************************)
(***** List Helpers *****)
(************************)

(** The list containing n replicas of e *)
let rec replicate (n:int) (e:'a) : 'a list =
  if n = 0 then [] else e::replicate (n-1) e

(** The cross produce of lists xs and ys.
    result = \[ (x,y) | x in xs and y in ys \] *)
let rec cross (xs:'a list) (ys:'b list) : ('a*'b) list =
  match xs with
  | [] -> []
  | hd::tl -> List.map ~f:(fun y -> (hd,y)) ys @ cross tl ys

(** The monotonically increasing list containing each number in the range
    between n1 and n2 (inclusive) *)
let rec range (n1:int) (n2:int) : int list =
  if n1 > n2 then [] else n1::range (n1+1) n2

(** The list of unique elements in xs. *)
let rec unique (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | hd::tl ->
      let tl' = unique tl in
      if List.mem tl' hd then tl' else hd::tl'

(**************************)
(***** Number Helpers *****)
(**************************)

(** Bound x between low and high. *)
let bound (low:int) (high:int) (x:int) : int = min (max low x) high

(********************************)
(***** Random Value Helpers *****)
(********************************)

(** call f with probability (1/p) and g if f is not called *)
let with_inv_probability_or (r:int->int) (p:int)
                            (f:unit->'a) (g:unit->'a) : 'a =
  if r p = 0 then f () else g ()

(** Call f with probability (1/p) (using r to generate random numbers) *)
let with_inv_probability (r:int->int) (p:int) (f:unit->unit) : unit =
  with_inv_probability_or r p f (fun () -> ())

(** Call one of the functions in the list with equal probability. *)
let with_equal_probability (r:int->int) (fs:(unit -> unit) list) : unit =
  (List.nth_exn fs (r (List.length fs))) ()
