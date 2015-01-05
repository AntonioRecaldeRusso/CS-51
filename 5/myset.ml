open Core.Std

(* Definitions for sets. *)

<<<<<<< HEAD
exception Impossible
=======
exception TODO
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Ordering.t
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
<<<<<<< HEAD
  (*open Order*)
=======
  open Order
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
<<<<<<< HEAD
  (*open Order*)
=======
  open Order
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Equal -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right xs ~f:insert ~init:ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Equal -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left ~f:(fun a x -> f x a) ~init:e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left s ~f:f ~init:"") ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left lst ~f:(fun r k -> insert k r) ~init:d

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter elts ~f:(fun k -> assert(member s1 k)) ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right elts ~f:(fun k r -> remove k r) ~init:s1 in
    List.iter elts ~f:(fun k -> assert(not (member s2 k))) ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
<<<<<<< HEAD
   ()
=======
    ()
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
<<<<<<< HEAD
   test_member () ;
=======
    test_member () ;
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)
<<<<<<< HEAD

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make
    (struct
      type key = C.t
      type value = unit
      let compare k1 k2 = C.compare k1 k2
      let string_of_key k = C.string_of_t k 
      let string_of_value _ = "()"
      let gen_key () = C.gen () 
      let gen_key_random ()= C.gen_random () 
      let gen_key_gt k = C.gen_gt k
      let gen_key_lt k = C.gen_lt k
      let gen_value () = ()
      let gen_pair () = (C.gen (), ()) 
      let gen_key_between k1 k2 = C.gen_between k1 k2
  end)
  
  (*Type for elements in the module*)
  type elt = D.key
  type set = D.dict
  
  (*Functions to match the signature*)
  
  (*The empty set*)
  let empty = D.empty
  
  (*Returns true if the set is empty*)
  let is_empty (s : set) : bool = (s = empty)
  
  (*Inserts and element k  ito a set d*)
  let insert   (k : elt ) (d: set) : set =  D.insert d k ()
  
  (*Inserts an element into the empty set*)
  let singleton (elem : elt ) : set = D.insert D.empty elem ()
  
  (*Takes two sets d1 and d2 and returns a set with the union of both*)
  let rec union (d1 : set) (d2 : set) : set = 
    match D.choose d1 with
    | None -> d2
    | Some (elem, (), d1') -> union d1' (D.insert d2 elem ())

  (*returns true if an element elem is contained into a set d*)
  let member (d : set) (elem : elt) : bool = D.member d elem
 
  (*Takes two sets d1, d2 and returns a new set with the elements in common *)
  let rec intersect (d1 : set) (d2 : set) : set= 
    match D.choose d1 with
    | None -> empty
    | Some (elem, (), nextd1) -> 
      if (member d2 elem) then insert elem (intersect nextd1 d2) 
      else (intersect nextd1 d2)  

  (* Removes an element elem from a set d and 
   * returns a new set with out the elemet *)
  let remove  (elem : elt) (d : set) : set = D.remove d elem
  
  (* Takes a set d and returns a pair with one element and 
   * a new set with out that element*)
  let choose (d : set) : (elt * set) option = 
  match D.choose d  with
  | None -> None
  | Some (elem, (), d1') -> Some (elem, d1')
  

  (*Implements the function fold to mathc the signature*)
  (*val fold : (elt -> 'a -> 'a) -> 'a -> set ->*)
  let rec fold f init d =  
    match choose d with
    | None -> init
    | Some (ele, d1) -> f ele (fold f init d1) 
  
 

  (* implement the rest of the functions in the signature! *)
  (*translate to strings the elements and sets *)
=======
(*
module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(struct
      ??? fill this in!
  end)

  type elt = D.key
  type set = D.dict
  let empty = ???

  (* implement the rest of the functions in the signature! *)

>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)
<<<<<<< HEAD
   
   (* Values to test the functions*)
   let key1 = C.gen () 
   let key2 = C.gen_gt key1 ()
   let key3 = C.gen_gt key2 ()
   let d1 = insert key1 empty 
   let d2 = insert key2 d1
   let d3 = insert key3 d2
   let d4 = insert key3 empty
   let d5 = insert key2 empty

   (* Tests *)
   let test_insert () =
     assert (remove key1 d1 = empty);
     assert (remove key3 d3 = d2);
     assert (remove key2 d2 = d1);
     ()

   let test_is_empty () =
     assert (is_empty (remove key1 d1));
     assert (is_empty empty);
     assert (not(is_empty d1));
     ()

   let test_singleton () =
     assert (singleton key1 = d1);
     ()
   
   let test_union () =
     assert (union d1 d2 = d2);
     let u = union d1 d2 in
     assert (member u (match choose d1 with | Some (x,_) -> x | None -> raise Impossible));
     ()
    

   let test_intersect () =
     assert (intersect d2 d3 = d2);
     assert (intersect d1 d3 = d1);
     assert (intersect empty empty = empty);
     assert (intersect empty d1 = empty);
     assert (intersect d1 empty = empty);
     assert (intersect d4 d5 = empty);
     ()
   
   let test_choose () =
     assert (choose d1 = Some (key1, empty));
     ()

   let test_fold () =
     let f (elt: elt) (init : string) = (string_of_elt elt) ^ " " ^ init in
     assert((fold f "" d3) = (string_of_elt key2) ^ " " ^ (string_of_elt key1) ^ " " ^ (string_of_elt key3) ^ " ")

  (* add your test functions to run_tests *)
  let run_tests () =
  test_insert() ;
  test_is_empty() ;
  test_singleton() ;
  test_union () ;  
  test_intersect() ;
  test_choose () ;
  test_fold () ;
  ()
end

=======

  (* add your test functions to run_tests *)
  let run_tests () =
    ()
end
*)
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067



(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)
<<<<<<< HEAD

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;

=======
(*
module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;
*)
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
<<<<<<< HEAD
  (*ListSet (C)*) 
  DictSet (C) 
=======
  ListSet (C)
  (* DictSet (C) *)
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067

