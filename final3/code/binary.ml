open Core.Std
open Order

exception EmptyTree
exception DontKnow


module type BINARYTREE =
sig
  type elt 
  type tree
  val empty : tree
  val search: elt -> tree -> bool
  val insert : elt -> tree -> tree
  val delete : elt -> tree -> tree
(*  val getmin : tree -> elt
  val getmax : tree -> elt *)
(* val sort : tree -> elt list *)
(*  val printbdf : tree -> unit *)
(*  val printdpf : tree -> unit *)
end


module BinaryTree(C : COMPARABLE) (* : BINARYTREE *) =
  struct
    type elt = C.t
    type tree = Leaf | Branch of tree * elt * tree
    
    let empty = Leaf

    let rec pull_max (t : tree) : elt * tree =
      match t with
      | Leaf -> failwith "empty tree"
      | Branch(Leaf,v,Leaf) -> (v, Leaf)
      | Branch(l,v,Leaf) -> (v, l)
      | Branch(l,v,r) -> 
        let v', t' = pull_max r in (v', (Branch(l,v,t')))
    
    (*Insert an element into a tree*)
    let rec insert (x: elt) (t : tree) : tree =
    match t with
    | Leaf -> Branch(Leaf, x, Leaf) 
    | Branch(l,e,r) -> 
      (match C.compare e x with
       | EQUAL -> t
       | LESS -> Branch((insert x l),e,r)
       | GREATER -> Branch(l,e,(insert x r)))
   
    (* returns true if x found in tree *)
    let rec search (x: elt) (t: tree): bool  = 
      match t with
      | Leaf -> false
      | Branch (l,e,r) -> 
        (match C.compare x e with
	 | EQUAL -> true
	 | LESS -> search x l
	 | GREATER -> search x r  )

    let rec delete (e : elt) (t : tree) : tree =
      match t with
      | Leaf -> Leaf
      | Branch(l,v,r) ->
        (match C.compare e v with
	 | EQUAL -> 
           if l = Leaf then r else
            let v', l' = pull_max l in
            Branch(l', v', r)
	 | LESS -> Branch((delete e l),v,r)
	 | GREATER -> Branch(l,v,(delete e r)))
    
    let rec getmin  (t : tree) = 
      match t with
      | Leaf -> raise DontKnow
      | Branch(Leaf, e, _) -> e
      | Branch(l, _, _) -> getmin l

    let rec getmax  (t : tree) = 
      match t with
      | Leaf -> raise DontKnow
      | Branch (_, e, Leaf) -> e
      | Branch (_, _, r) -> getmax r
(*
    let rec printbdf (t : tree) : unit =
      match t with
      | Leaf -> print_string "Leaf "
      | Branch(l, e, r) -> 
        printbdf l; C.to_string e; print_string " "; printbdf r

    let rec printdpf (t : tree) : unit =
      match t with
      | Leaf -> print_string "Leaf "
      | Branch(l,e,r) ->
        C.to_string e; printdpf l; printdpf r 
 *)
     
    
  end 

(* 
module BT = BinaryTree(IntCompare)

let x = BT.empty   (* need new tests *)
let y = BT.insert 8 x
let z = BT.insert 10 y
let smalltree = BT.insert 4 z
let a = BT.insert 3 smalltree
let b = BT.insert 5 a
let c = BT.insert 9 b
let mediumtree = BT.insert 11 c
let _ = print_string "\n"
let _ = BT.printbdf mediumtree
let _ = print_string "\n\n"
 *)              
