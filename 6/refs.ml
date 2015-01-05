(* NAMES:
 *
 * Partner 1's name: Antoio Recalde Russo
 * Partner 1's code.seas account: ~recalderusso
 *
 * (Leave blank if you are working alone)
 * Partner 2's name: Rafael Flores Gonzalez
 * Partner 2's code.seas account: ~rafaelfloresgonzalez
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)



(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)

(* Returns true if a mlist element "target" is "lst", false otherwise *)   
let rec mmember (lst : 'a mlist) (target : ('a mlist) ref) : bool  = 
  match lst with
  | Nil -> false
  | Cons(_, next) -> if (phys_equal next target) then true 
                     else mmember !next target 

(* Returns true if the argument has a cicle *)
let rec has_cycle (lst : 'a mlist) : bool =
  match lst with
  | Nil -> false
  | Cons(_, next) -> match !next with
		     | Nil -> false
                     (* if next is a member of !next2 we have a cycle*)
		     | Cons(_, next2) -> (mmember !next2 next) || has_cycle !next2 

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)
let emptylist = Nil

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

let r = ref Nil
let l1 = Cons(1,r)
let _ = r := l1


let () = assert (not(has_cycle list1a))
let () = assert (not(has_cycle list1b))
let () = assert (not(has_cycle list1))
let () = assert (has_cycle list2)

(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)

(* Nils the tail value of "lst" when it is equal to "target" *)
let rec member_flaten (lst : 'a mlist) (target : ('a mlist) ref) =
  match lst with
  | Nil -> ()
  | Cons(_, tl) -> if (phys_equal tl target) then tl := Nil 
                    else member_flaten !tl target

(* Removes the cycles from an mlist *)
let rec flatten (lst : 'a mlist) : unit =     
  match lst with
  | Nil -> ()
  | Cons(_, next) as cons ->  
    (match !next with
     | Nil -> ()
     | Cons(_, next2) -> member_flaten cons next2; flatten !next2)

let () = assert (has_cycle list2)
let () = flatten list2
let () = assert (not (has_cycle list2))				        
    

               


(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)

(*  Returns true if "target" is member of "lst". Works with lists  *)
let rec member lst target =
  match lst with
  | [] -> false
  | x :: xs -> if (phys_equal x target) then true
  else member xs target

let mlength (lst : 'a mlist) : int =
  (* Keeps counting nodes until a cycle starts or the mlist ends *)
  let rec count new_list n tmp =
    match new_list with
    | Nil -> n
    | Cons(_,y) -> if (member tmp !y) then n
                   else count !y (n + 1) (!y :: tmp) in
  count lst 0 []
;;


let r = ref Nil
let l1 = Cons(1,r)
let _ = r := l1

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

let () = assert (mlength list1a = 1)
let () = assert (mlength list1b = 2)
let () = assert (mlength list1 = 3)
let () = assert (mlength l1 = 1)
let () = assert (mlength list2 = 2)

(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 240
