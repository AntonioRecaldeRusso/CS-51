open Core.Std
open Order
open Rbt
open Avl
open Binary
open Bheap
open Splay
open Two_three

exception BadArgument

let argv = Sys.argv
if (Array.length argv <> 3) then raise BadArgument

let sn = Array.get argv 1
let n = Int.of_string sn


module SP = SplayTree (IntCompare)
module RB = RBT (IntCompare)
module AV = AVLTree (IntCompare)
module BI = BinaryTree (IntCompare)
module HE = BHEAP (IntCompare)
module TT = TwoThreeTree(IntCompare);;

let rec generate_list (n : int) : int list =
  match n with 
  | 0 -> []
  | x -> x :: generate_list (n  -1)


let gen_rand_list (n : int) (range : int) =
  let rec gen (l ) (n : int) =
    if n > 0 then 
      gen ((Random.int range) :: l) (n-1)
    else l in
    gen [] n


let rec list_insert_BI (l: int list) (t: BI.tree) : BI.tree =
  match l with
  |[] -> t
  |hd :: tl ->  list_insert_BI tl (BI.insert hd t)

let rec list_insert_RB (l : int list) (t : RB.tree) : RB.tree =
  match l with
  |[] -> t
  |hd :: tl ->  list_insert_RB tl (RB.insert hd t)

let rec list_insert_AV (l : int list) (t : AV.tree) : AV.tree =
  match l with
  |[] -> t
  |hd :: tl ->  list_insert_AV tl (AV.insert hd t)

let rec list_insert_SP (l : int list) (t : SP.tree) : SP.tree =
  match l with
  |[] -> t
  |hd :: tl ->  list_insert_SP tl (SP.insert hd t)


let rec list_insert_HE (l : int list) (t : HE.tree) : HE.tree =
  match l with
  |[] -> t
  |hd :: tl ->  list_insert_HE tl (HE.insert hd t)


let rec list_insert_TT (l : int list) (t : TT.dict) : TT.dict =
  match l with
  |[] -> t
  |hd :: tl ->  list_insert_TT tl (TT.insert t hd)


let l =
  match Array.get argv 2 with
  | "r" -> gen_rand_list n (n)
  | "o" -> generate_list n
  | "ns" -> gen_rand_list n ((n/1000) + 2)
  | _ -> raise BadArgument

(*

let l = if (Pervasives.compare (Array.get argv 2)  "r" = 0 )
  then (gen_rand_list n (n))
  else if (Pervasives.compare (Array.get argv 2) "o" = 0 )
  then (generate_list n) 
  else raise BadArgument 

let l2 =  if (Pervasives.compare (Array.get argv 2)  "r" = 0 )
  then (gen_rand_list n ((n/10) + 2))
  else if (Pervasives.compare (Array.get argv 2) "o" = 0 )
  then (generate_list n) 
  else raise BadArgument 

 *)

let test_rb = list_insert_RB l RB.empty
let test_av = list_insert_AV l AV.empty
(*let test_bi = list_insert_BI l BI.empty*)
let test_he = list_insert_HE l HE.empty
let test_sp = list_insert_SP l SP.empty
let test_tt = list_insert_TT l TT.empty
let test_sp_repeated = list_insert_SP l2 SP.empty

let t_i_rb =
let t = Unix.gettimeofday () in
let _ = RB.insert (n + 1) test_rb in
let t_i_rb = Unix.gettimeofday () -. t in
t_i_rb

let t_s_rb =
let t = Unix.gettimeofday () in
let _ = RB.search (n) test_rb in
let t_s_rb = Unix.gettimeofday () -. t in
t_s_rb



let t_i_av =
let t = Unix.gettimeofday () in
let _ = AV.insert (n + 1) test_av in
let t_i_av = Unix.gettimeofday () -. t in
t_i_av


let t_s_av =
let t = Unix.gettimeofday () in
let _ = AV.search (n) test_av in
let t_s_av = Unix.gettimeofday () -. t in
t_s_av


let t_i_sp =
let t = Unix.gettimeofday () in
let _ = SP.insert (n + 1) test_sp in
let t_i_sp = Unix.gettimeofday () -. t in
t_i_sp


let t_s_sp =
let t = Unix.gettimeofday () in
let _ = SP.search (n) test_sp in
let t_s_sp = Unix.gettimeofday () -. t in
t_s_sp


let t_s_sp_esp =
let t = Unix.gettimeofday () in
let _ = SP.search (n) test_sp_repeated in
let t_s_sp_esp = Unix.gettimeofday () -. t in
t_s_sp_esp

(*let t_i_bi =
let t = Unix.gettimeofday () in
let _ = BI.insert (n + 1) test_bi in
let t_i_bi = Unix.gettimeofday () -. t in
t_i_bi *)

let t_i_he =
let t = Unix.gettimeofday () in
let _ = HE.insert (n + 1) test_he in
let t_i_he = Unix.gettimeofday () -. t in
t_i_he

let t_s_he =
let t = Unix.gettimeofday () in
let _ = HE.search (n) test_he in
let t_s_he = Unix.gettimeofday () -. t in
t_s_he


let t_i_tt =
let t = Unix.gettimeofday () in
let _ = TT.insert  test_tt (n + 1)in
let t_i_tt = Unix.gettimeofday () -. t in
t_i_tt

let t_s_tt =
let t = Unix.gettimeofday () in
let _ = TT.search test_tt (n) in
let t_s_tt = Unix.gettimeofday () -. t in
t_s_tt

(*let time f =
  let t = Unix.gettimeofday () in
    let _ = f in
      let tf = Unix.gettimeofday () -. t in
        tf

let tinsertrbt = time (RB.insert (n + 1) test_rb)
let tinsertavl = time (AV.insert (n + 1) test_av)*)

let _ = print_string ("RBT   ins " ^ (Pervasives.string_of_float t_i_rb) ^ " Search " ^ (Pervasives.string_of_float t_s_rb) ^ "\n")
let _ = print_string ("AVL   ins " ^ (Pervasives.string_of_float t_i_av) ^ " Search " ^ (Pervasives.string_of_float t_s_av) ^ "\n")
(*let _ = print_string ("Binary insert " ^ (Pervasives.string_of_float t_i_bi) ^ "\n") *)
let _ = print_string ("HEAP  ins " ^ (Pervasives.string_of_float t_i_he) ^ " Search " ^ (Pervasives.string_of_float t_s_he) ^ "\n")
let _ = print_string ("SPLAY ins " ^ (Pervasives.string_of_float t_i_sp) ^ " Search " ^ (Pervasives.string_of_float t_s_sp) ^ "\n")
let _ = print_string ("2-3   ins " ^ (Pervasives.string_of_float t_i_tt) ^ " Search " ^ (Pervasives.string_of_float t_s_tt) ^ "\n")
