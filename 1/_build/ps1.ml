(*** CS 51 Problem Set 1 ***)
(*** January 31, 2014 ***)
(*** Antonio Recalde ***)

(* Open up the library we'll be using in this course *)
open Core.Std

(* Problem 1 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(*>* Problem 1a *>*)

let prob1a : string = let greet y = "Hello " ^ y in greet "World!";;


(*>* Problem 1b *>*)

let prob1b : int option list = [Some 4; Some 2; None; Some 3];;


(*>* Problem 1c *>*)

let prob1c : ('a option * float option) * bool = ((None, Some 42.0), true);;



(* Explain in a comment why the following will not type check,
   and provide a fix *)

(*>* Problem 1d *>*)
(*
let prob1d : string * int list = [("CS", 51); ("CS", 50)];;

-----> It does not evaluate because the declaration of list of pairs needs 
       to have the pairs parenthesized                               <----- *)
let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];; 


(*>* Problem 1e *>*)
(*
let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4, 3.9) then 4 else 2;;

--> It does not type check because the expression was comparing (int < float),
    which is not possible in OCaml                                     <--- *)

let prob1e : int =
  let compare (x, y) = x < y in
  if compare (4., 3.9) then 4 else 2;;        (* How to fix: change 4 to 4. *)


(*>* Problem 1f *>*)
(*
let prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); ("April", None);
   ("May", None); ("June", 1); ("July", None); ("August", None);
   ("September", 3); ("October", 1); ("November", 2); ("December", 3)] ;;

   -----> ANSWER:  It does not type check because sometimes there are ints, 
          sometimes there is none.. It is an int option, not a string.   <---
*)
let prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); ("April", None);
   ("May", None); ("June", Some 1); ("July", None); ("August", None);
   ("September", Some 3); ("October", Some 1); ("November", Some 2); 
   ("December", Some 3)] ;;




(* Problem 2 - Write the following functions *)
(* For each subproblem, you must implement a given function and corresponding
 * unit tests (i.e. assert expressions). You are provided a high level
 * description as well as a prototype of the function you must implement. *)

(*>* Problem 2a *>*)

(* `reversed list` should return true if the integers in list are in
 * decreasing order. The empty list is considered to be reversed. Consecutive
 * elements can be equal in a reversed list. *)

(* Here is its prototype/signature: *)
(* reversed : int list -> bool *)

(* Implement reversed below, and be sure to write tests for it (see 2b for
 * examples of tests). *)


let rec reversed (list : int list) : bool = 
   match list with
   | [] -> true
   | [_] -> true
   | [x; y] -> if x >= y then true else false
   | first :: second :: the_rest ->
      if first >= second then reversed (second :: the_rest) else false
;;  


assert (reversed [] = true);;
assert (reversed [1] = true);;
assert (reversed [1; 2] = false);;
assert (reversed [1; 2; 3; 4; 5; 6; 7] = false);;
assert (reversed [7; 6; 5; 4; 3; 1; 2] = false);;
assert (reversed [2; 2; 2; 1; 1; 1] = true);;
assert (reversed [4; 3; 2; 1] = true);;
assert (reversed [2; 1] = true);;
assert (reversed [2; 2] = true);;






(*>* Problem 2b *>*)

(* merge takes two integer lists, each sorted in increasing order,
 and returns a single merged list in sorted order. For example:

merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]
*)

(* The type signature for merge is as follows: *)
(* merge : int list -> int list -> int list *)

(* The following function merges a list as by specifications. While
 * the style guideline prefers "if then else" in the same line, 
 * in this it wasn't as readable in line 140. Hence different lines  *)
let rec merge (list1 : int list) (list2 : int list) : int list = 
   match list1, list2 with
   | [], l -> l
   | l', [] -> l'
   | (hd1 :: tl1), (hd2 :: tl2) -> 
      if hd1 <= hd2        
      then hd1 :: merge tl1 list2 
      else hd2 :: merge list1 tl2
;;


let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;



(*>* Problem 2c *>*)
(* unzip should be a function which, given a list of pairs, returns a
 * pair of lists, the first of which contains each first element of
 * each pair, and the second of which contains each second element.
 * The returned lists should have the elements in the order in which
 * they appeared in the input. So, for instance:

unzip [(1,2);(3,4);(5,6)];;
- : int list * int list = ([1;3;5],[2;4;6])

*)
(* The type signature for unzip is as follows: *)
(* unzip : (int * int) list -> int list * int list) *)


let rec unzip (pairs_list : (int * int) list) : int list * int list = 
  match pairs_list with
  | [] -> ([], [])
  | [(x, y)] -> ([x], [y])
  | (x, y) :: tail -> 
     let (a, b) = unzip tail in ( x :: a, y :: b )
;;

assert( (unzip []) =  ([],[]) );;
assert( (unzip [(1,2)]) =  ([1],[2]) );;
assert( (unzip [(1,2);(3,4)]) =  ([1;3],[2;4]) );;
assert( (unzip [(1,2);(3,4);(5,6)]) =  ([1;3;5],[2;4;6]) );;
assert( (unzip [(1,2);(3,4);(5,6);(7,8)]) =  ([1;3;5;7],[2;4;6;8]) );;

 

(*>* Problem 2d *>*)

(* `variance lst` returns None if lst has fewer than 2 floats, and
 * Some of the variance of the floats in lst otherwise.  Recall that
 * the variance of a sequence of numbers is 1/(n-1) * sum (x_i-m)^2,
 * where a^2 means a squared, and m is the arithmetic mean of the list
 * (sum of list / length of list). For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0];;
- : int option = Some 2.5
variance [1.0];;
- : int option = None

 * Remember to use the floating point version of the arithmetic
 * operators when operating on floats (+. *., etc). The "float"
 * function can cast an int to a float. *)

(* variance : float list -> float option *)


let variance (lst : float list) : float option =
  (* sum_floats returns a Summation of the elements of a list  *)
  let rec sum_floats (lst : float list) : float =
    match lst with
    | [] -> 0.
    | [x] -> x
    | hd :: tl -> hd +. sum_floats tl in
  (* length_list returns the length of a list  *)
  let rec length_list (lst : float list) : float =
    match lst with
    | [] -> 0.
    | [_] -> 1.
    | _ :: tl -> 1. +. length_list tl in		  
  (* find_mean computes the mean of elements of in a list *)
  let find_mean (lst : float list) : float =
    match lst with
    | [] -> 0.
    | [x] -> x
    | _ :: _ -> sum_floats lst /. length_list lst in
  (* this function substracts list elements by their mean. Then squares them. *)
  let rec s_and_sq (lst : float list) (value : float) : float list =
    (match lst with
    | [] -> []
    | [x] -> (x -. value) *. (x -. value) :: []
    | hd :: tl -> (hd -. value) *. (hd -. value) :: s_and_sq tl value) in
  match lst with
  | [] -> None
  | [_] -> None
  | _ :: _ -> 
    (* 1/(n-1) * sum (x_i-m)^2 *)
    let result = ( Some ( 1. /. (length_list lst -. 1.) *. 
		      sum_floats (s_and_sq lst (find_mean lst)) )) in
      if result = Some 0. then None else result
;;

assert( variance [1.0; 2.0; 3.0; 4.0; 5.0] = Some 2.5 );;
assert( variance [1.0] = None );;
assert( variance [1.0; 1.0; 1.0; 1.0] = None );;
assert( variance [5.0; 7.0; 3.; 4.5; 6.] = Some 2.3 );;
assert( variance [161.0; 42.0; 33.0; 14.0; 95.0] = Some 3547.5 );;


  
(*>* Problem 2e *>*)

(* few_divisors n m should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise. Note that this is *not* the
 * same as n having fewer divisors than m:

few_divisors 17 3;;
- : bool = true
few_divisors 4 3;;
- : bool = false
few_divisors 4 4;;
- : bool = true

 * Do not worry about negative integers at all. We will not test
 * your code using negative values for n and m, and do not
 * consider negative integers for divisors (e.g. don't worry about
 * -2 being a divisor of 4) *)

(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)


(* Calculates divisors via (exponents + 1) and then multiplying 
 * resulting values together. E.g.:
 * 2^3 * 3^2 * 5^1 -> [3;2;1] -> [4;3;2] -> 24 ...360 has 24 divisors.   *)
let few_divisors (value : int) (divs : int) : bool =
  (* returns a list of prime factors. The argument num = current divisor *)
  let rec get_prime_factors (value : int) (num : int) : int list =
    if value = 1 then []
    else if value mod num = 0 then num :: get_prime_factors (value / num) num
    else get_prime_factors value (num + 1) in
  (* Counts level of exponentiation. Stores in list. Ex: (2^3 * 3^1) -> [3;1] *)
  let rec get_exponents (lst : int list) (pow : int ) (tmp : int list) =
    let rec appnd target item =
      match target with
      | [] -> [item]
      | hd :: tl -> hd :: appnd tl item in
    match lst with
    | [] -> []
    | [_] -> appnd tmp pow  
    | hd1 :: hd2 :: tl -> 
       if hd1 = hd2 
       then get_exponents (hd2 :: tl) (pow + 1) tmp
       else get_exponents (hd2 :: tl) 1 (appnd tmp pow) in
  (* Increments each elements of the list by 1, then multiplies them together *)
  let process_divisors (lst : int list) : int =
    let rec increment_each new_list = 
      match new_list with
      | [] -> []
      | hd :: tl -> (hd + 1) :: increment_each tl in
    let rec multiply_list new_list2  =
      match new_list2 with
      | [] -> 1
      | hd :: tl -> hd * multiply_list tl in
    match lst with
    | [] -> 0
    | _ :: _ -> multiply_list (increment_each lst) in
  (* arg[2] = 2 because 2 is the smallest relevant divisor *)
  let factors = get_prime_factors value 2 in
  let divisors = process_divisors (get_exponents factors 1 []) in
  if divisors < divs then true else false
;;

assert( few_divisors 17 3 = true );;
assert( few_divisors 4 3 = false );;
assert( few_divisors 4 4 = true );;
assert( few_divisors 10 4 = false );;
assert( few_divisors 10 5 = true );;
assert( few_divisors 12345 7 = false );;
assert( few_divisors 12345 9 = true );;
assert( few_divisors 1 0 = false );;
  

(*>* Problem 2f *>*)

(* `concat_list sep lst` returns one big string with all the string
 * elements of lst concatenated together, but separated by the string
 * sep. Here are some example tests:

concat_list ", " ["Greg"; "Anna"; "David"];;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"];;
- : string = "Moo...Baaa...Quack"
concat_list ", " [];;
- : string = ""
concat_list ", " ["Moo"];;
- : string = "Moo"

*)

(* The type signature for concat_list is: *)
(* concat_list : string -> string list -> string *)

let rec concat_list (sep : string) (lst : string list) : string =
  match lst with
  | [] -> ""
  | [x] -> x
  | hd :: tl -> hd ^ sep ^ concat_list sep tl
;;


assert( concat_list ", " ["Greg"; "Anna"; "David"] = "Greg, Anna, David" );;
assert( concat_list "..." ["Moo"; "Baaa"; "Quack"] = "Moo...Baaa...Quack" );; 
assert( concat_list ", " [] = "" );;
assert( concat_list ", " ["Moo"] = "Moo" );;
assert( concat_list "...Mississipi  " ["One"; "Two"; "Three.."] =
	  "One...Mississipi  Two...Mississipi  Three.." );;


(*>* Problem 2g *>*)

(* One way to compress a list of characters is to use run-length encoding.
 * The basic idea is that whenever we have repeated characters in a list
 * such as ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] we can
 * (sometimes) represent the same information more compactly as a list
 * of pairs like [(5,'a');(3,'b');(1,'c');(4,'d')].  Here, the numbers
 * represent how many times the character is repeated.  For example,
 * the first character in the string is 'a' and it is repeated 5 times,
 * followed by 3 occurrences of the character 'b', followed by one 'c',
 * and finally 4 copies of 'd'.
 *
 * Write a function to_run_length that converts a list of characters into
 * the run-length encoding, and then write a function from_run_length
 * that converts back. Writing both functions will make it easier to
 * test that you've gotten them right. *)

(* The type signatures for to_run_length and from_run_length are: *)
(* to_run_length : char list -> (int * char) list *)
(* from_run_length : (int * char) list -> char list *)


let to_run_length (raw_list : char list) : (int * char) list =
  (* Counts how many consecutive chars are in list, then stores in a pair    *)
  let rec count_char (lst : char list) (count : int ) (tmp : (int * char) list) =
    let rec appnd target item =
      match target with
      | [] -> [item]
      | hd :: tl -> hd :: appnd tl item in
    match lst with
    | [] -> []
    | [x] -> appnd tmp (count, x)  
    | hd1 :: hd2 :: tl -> 
       if hd1 = hd2 then count_char (hd2 :: tl) (count + 1) tmp
       else count_char (hd2 :: tl) 1 (appnd tmp (count, hd1)) in
  match raw_list with
  | [] -> []
  | [x] -> [(1, x)]
  | _ :: _ -> count_char raw_list 1 []
;;

(* Reads from pair and recursively adds the letters to a list  *)
let rec from_run_length (compressed_list : (int * char) list) : char list =
  let rec add_to_list times letter =
    match times with
    | 1 -> letter :: []
    | n -> letter :: add_to_list (n - 1) letter in 
  match compressed_list with
  | [] -> []
  | [(num, ch)] -> add_to_list num ch
  | (num', ch') :: tl -> 
     (* appends a list to another *)
     let rec merge = function
       | [], tail -> tail
       | (hd :: tl), tail -> hd :: merge(tl, tail) in
     merge( add_to_list num' ch', from_run_length tl )
;;  


assert( to_run_length [] = [] );;
assert( to_run_length ['a'] = [(1,'a')] );;
assert( to_run_length ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] =
	  [(5,'a');(3,'b');(1,'c');(4,'d')] );;
assert( from_run_length [] = [] );;
assert( from_run_length [(1,'a')] = ['a'] );;
assert( from_run_length [(5,'a');(3,'b');(1,'c');(4,'d')] =
	  ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] );;
assert( from_run_length (to_run_length ['a';'a';'b';'b']) = ['a';'a';'b';'b']);;


(*>* Problem 3 *>*)

(* Challenge!

 * permutations lst should return a list containing every
 * permutation of lst. For example, one correct answer to
 * permutations [1; 2; 3] is
 * [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]].

 * It doesn't matter what order the permutations appear in the returned list.
 * Note that if the input list is of length n then the answer should be of
 * length n!.
 * Hint:
 * One way to do this is to write an auxiliary function,
 * interleave : int -> int list -> int list list,
 * that yields all interleavings of its first argument into its second:
 * interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ].
 * You may also find occasion for the library functions
 * List.map and List.concat. *)

(* The type signature for permuations is: *)
(* permutations : int list -> int list list *)


(* This function returns a list of permutations of an int list:
 * args: original   The list to be permutted *)
let permutations (original : int list) : int list list =
  match original with
  | [x] -> [[x]]
  | _ ->
     (* buble works in a similar way as bubble-sort.. it pushes the first 
        number of the list until it reaches the end, at which point it starts
        over with a different number.
        args: 
          sub: The part of the list to be analyze in the next recursive step
          super:   The part of the list not relevant in this recursive step
          tmp:   The current state of our return list                          *)
     let rec bubble (sub : int list) (super : int list) (tmp : int list list) =
       match sub with
       | [] -> []
       | [_] -> if List.length tmp > 1 && super@sub = original then tmp
		else let rec last (x : int list list) : int list =
		       match x with
		       | [] -> []
		       | hd :: [] -> hd
		       | _ :: tl -> last tl in
		     bubble (last tmp) [] tmp
       | hd1 :: hd2 :: tl -> 
	  bubble (hd1 :: tl) (super@[hd2])  (tmp@[(super@[hd2])@(hd1 :: tl)]) in
     bubble original [] []
;;


(* Testing with lists of lengths 0-4..    *)

assert( permutations [] = [] );;
assert( permutations [1] = [[1]] );;
assert( permutations [1;2] = [[2; 1]; [1; 2]] );;
assert( permutations [1;2;3] = 
	  [[2; 1; 3]; [2; 3; 1]; [3; 2; 1]; [3; 1; 2]; [1; 3; 2]; [1;2;3]] );;
assert( permutations [1;2;3;4] =
	  [[2; 1; 3; 4]; [2; 3; 1; 4]; [2; 3; 4; 1]; [3; 2; 4; 1];
	   [3; 4; 2; 1]; [3; 4; 1; 2]; [4; 3; 1; 2]; [4; 1; 3; 2]; [4; 1; 2; 3];
	   [1; 4; 2; 3]; [1; 2; 4; 3]; [1; 2; 3; 4]] );;
