
open Core.Std

(* TESTING REQUIRED:
 * Ocaml provides the function "assert" which takes a bool. It does nothing if
 * the bool is true and throws an error if the bool is false.
 *
 * To develop good testing practices, we expect at least 2 tests (using assert)
 * per function that you write on this problem set. Please follow the model
 * shown in 1.1.a, putting the tests just below the function being tested.
 *)


(* TIME ADVICE:
 * Part 2 of this problem set (expression.ml) can be difficult, so be careful
 * with your time. *)

(****************************************************)
(******       1.1: Sparking your INTerest      ******)
(****************************************************)

(* Solve each problem in this part using List.map, List.fold_right, or
 * List.filter.
 *
 * See the Ocaml Core Library documentation on lists:
 * List.map ~f:(str_concat) listshttps://ocaml.janestreet.com/ocaml-core/109.60.00/doc/core/#Std.List
 *
 * A solution, even a working one, that does not use one of these
 * higher-order functions, will receive little or no credit.
 * However, if you can express your solution to
 * one particular part in terms of another function from
 * another part, you may do so.
 *
 * You MAY NOT change the definition of these
 * functions to make them recursive. *)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list = List.map ~f:(( * ) (-1)) nums
;;

(* Unit test example. *)
assert( negate_all [1; -2; 0] = [-1; 2; 0]) ;;
assert( negate_all [] = []);;


(*>* Problem 1.1.b *>*)

(*  sum : Returns the sum of the elements in the list. *)
let sum (nums:int list) : int = List.fold_right ~f:(+) ~init:0 nums
;;

assert( sum [1;2;3;4] = 10 );;
assert( sum [] = 0 );;
assert( sum [1; -2; 5; -10] = -6 );;

(*>* Problem 1.1.c *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list = 
  List.map ~f:(List.fold_right ~f:(+) ~init:0) rows
;;

assert( sum_rows [[1;2;3];[2;2;2];[3;3;3]] = [6;6;9] );;
assert( sum_rows [[];[];[1]] = [0;0;1] );;
assert( sum_rows [[1;2]; [3;4]] = [3; 7] );;
assert( sum_rows [] = [] );;
(*>* Problem 1.1.d *>*)

(*  filter_odd : Retains only the odd numbers from the given list.
 *     Example : filter_odd [1;4;5;-3] = [1;5;-3]. *)
let filter_odd (nums:int list) : int list = 
  List.filter ~f:(fun x -> x mod 2 <> 0) nums
;;

assert( filter_odd [1;4;5;-3] = [1;5;-3] );;
assert( filter_odd [2;4;6;8] = [] );;
assert( filter_odd [] = [] );;


(*>* Problem 1.1.e *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n : int) (nums : int list) : int =
  List.fold_right 
    ~f:(+) ~init:0 ( List.map ~f:(fun x -> if x = n then 1 else 0 ) nums);;

(* alternate solution:
let num_occurs (n:int) (nums:int list) : int = 
  List.length(List.filter ~f:(fun x -> x = n) nums);; 
*)

assert( num_occurs 4 [1;3;4;5;4] = 2 );;
assert( num_occurs 0 [1;4;5;4] = 0 );;
assert( num_occurs 1 [] = 0 );;
assert( num_occurs 7 [7;7;7;7;7;7;7] = 7 );;

(*>* Problem 1.1.f *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
  List.fold_right 
    ~f:(+) ~init:0 (List.map ~f:( List.fold_right ~f:(+) ~init:0 ) nlists);;

assert( super_sum [[1;2;3];[];[5]] = 11 );;
assert( super_sum [] = 0 );;
assert( super_sum [[-3];[3];[2];[-2]] = 0 );;

(*>* Problem 1.1.g *>*)

(*  filter_range : Returns a list of numbers in the input list within a
 *                 given range (inclusive), in the same order they appeared
 *                 in the input list.
 *       Example : filter_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let filter_range (nums:int list) (range:int * int) : int list =
  let (a, b) = range in
  List.filter ~f:(fun x -> x >= a && x <= b) nums;;

assert( filter_range [1;3;4;5;2] (1,3) = [1;3;2] );;
assert( filter_range [1;2;3;4;5] (2,4) = [2;3;4] );;
assert( filter_range [1;2;3;4;5] (4,2) = [] );;
assert( filter_range [] (0,0) = [] );;
assert( filter_range [(-1);0;1;2;3] ((-1),1) = [(-1);0;1] );;


(****************************************************)
(**********       1.2 Fun with Types       **********)
(****************************************************)


(*>* Problem 1.2.a *>*)

(*  floats_of_ints : Converts an int list into a list of floats *)
let floats_of_ints (nums:int list) : float list =
  List.map ~f:(fun x -> Float.of_int x) nums
;;

assert( floats_of_ints [3;2;1] = [3.;2.;1.] );;
assert( floats_of_ints [0;0] = [0.;0.] );;
assert( floats_of_ints [-1] = [-1.] );;
assert( floats_of_ints [] = [] );;


(*>* Problem 1.2.b *>*)

(*   log10s : Applies the log10 function to all members of a list of floats.
 *            The mathematical function log10 is not defined for
 *            numbers n <= 0, so undefined results should be None.
 *  Example : log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None] *)
let log10s (lst: float list) : float option list =
  List.map ~f:(fun n -> if n <= 0. then None else Some (log10 n)) lst
;;


(* This function was made for the purpose of testing log10s
   It takes a sample (l1) and a control list (l2). It returns true if 
   the difference in value in each list (in parallel analysis) is smaller 
   than epsilon (eps) *)
let rec compare_lists (l1:float option list) (l2:float option list) (eps:float) =
  match l1, l2 with
  | [], []         -> true
  | [], _  | _, [] -> false
  | x::xs, y::ys   -> 
    (match x, y with
     | None, None -> true && compare_lists xs ys eps
     | None, _ 
     | _, None    -> false
     | Some x, Some y  -> 
       (Float.abs( x -. y) <= eps) && (compare_lists xs ys eps))
;;


assert(compare_lists (log10s [1.0;10.0;(-10.0)]) [Some 0.;Some 1.;None] 0.001);;
assert(compare_lists (log10s [2.0; 1.5;15.0;3.0]) 
		     [Some 0.3010;Some 0.1760;Some 1.1760;Some 0.4771] 0.001 );;
assert(compare_lists (log10s []) [] 0.001 );;



(*>* Problem 1.2.c *>*)

(*  deoptionalize : Extracts values from a list of options.
 *        Example : deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] *)

let deoptionalize (lst:'a option list) : 'a list =
  List.map 
    ~f:(fun x -> match x with                   (* First argument -function- *)
                 | Some x -> x 
                 | None -> raise (Failure("Impossible"))) 
    (List.filter ~f:(is_some) lst)              (* Second argument -list-    *)
;;

assert( deoptionalize [None] = [] );;
assert( deoptionalize [] = [] );;
assert( deoptionalize [Some (Some 3); Some None;] = [Some 3; None] );;
assert( deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] );;

(*>* Problem 1.2.d *>*)

(*  some_sum : Sums all of the numbers in a list of int options;
 *             ignores None values *)
let some_sum (nums:int option list) : int =
  List.fold_right ~f:(+) ~init: 0 (deoptionalize nums)
;;

assert( some_sum [None; Some 1; Some 2; Some 3; Some 4] = 10 );;
assert( some_sum [] = 0 );;
assert( some_sum [None] = 0 );;
assert( some_sum [Some 1] = 1);;

(*>* Problem 1.2.e *>*)

(*  mult_odds : Product of all of the odd members of a list.
 *    Example : mult_odds [1;3;0;2;-5] = -15 *)
let mult_odds (nums:int list) : int =
  List.fold_right 
    ~f:( * ) ~init:1 (List.filter ~f:(fun x -> abs (x mod 2) = 1) nums)
;;

assert( mult_odds [1;3;0;2;-5] = -15 );;
assert( mult_odds [2;4;6] = 1 );;
assert( mult_odds [] = 1 );;   (* in piazza this was said to be right. *)
assert( mult_odds [3] = 3 );;
(*>* Problem 1.2.f *>*)

(*  concat : Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list =
  List.fold_left ~f:(@) ~init:[] lists;;


assert( concat [[true;false];[false;true]] = [true;false;false;true] );;
assert( concat [[1;2];[3];[4;5]] = [1;2;3;4;5] );;
assert( concat [["Hello"];[];["World!"; "This is OCaml"]] = 
	  ["Hello"; "World!"; "This is OCaml"] );;


(*>* Problem 1.2.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year
;;

(*  filter_by_year : returns the names of the students in a given year
 *         Example : let students = [("Joe",2010);("Bob",2010);("Tom",2013)];;
 *                   filter_by_year students 2010 => ["Joe";"Bob"] *)
let filter_by_year (slist:student list) (yr:year) : name list =
  List.map ~f:(fun (n, _) -> n) (List.filter ~f:(fun ( _, y) -> y = yr) slist)
;;

assert( filter_by_year [("Joe",2010);("Bob",2010);("Tom",2013)] 2010 =
	  ["Joe";"Bob"] );;
assert( filter_by_year [("student1", 2000);("student2",2001)] 2000 =
	  ["student1"] );;
assert( filter_by_year [] 2001 = [] );;


(*>* Problem 1.3 *>*)

(* Please give us an honest estimate of how long this Part of the problem
 * set took you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent_on_part_1 : int = 400;;
