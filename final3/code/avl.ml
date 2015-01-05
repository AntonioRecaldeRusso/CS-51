(* Meaning of labels:
 * e = element, t = tree, h = height, v = value, 
 * l = left, r = right, hl = height left, hr = height right  
 *)

open Order

exception InvariantBroken
exception EmptyTree

module AVLTree(C : COMPARABLE) (* : BINARYTREE *)=
struct
  type elt = C.t
  
  (* The int keeps track of the height of the current node *)
  type tree = Leaf | Branch of int * elt * tree * tree
  
  let empty : tree = Leaf

  let isEmpty (t : tree) : bool = (t = Leaf)

  (* returns the height of the current branch *)
  let get_height (t : tree) =
    match t with
    | Leaf -> 0
    | Branch(h,_,_,_) -> h

  (* calculates the balance based on the subtrees' heights *)
  let get_balance (t : tree)  : int =
    match t with
    | Leaf -> 0
    | Branch(_,_,l,r) -> (get_height l) - (get_height r)
 

  
  let balance (t : tree) : tree =
    match t with
    | Leaf -> failwith "t error"
    | Branch(_,v,l,r) ->
      (* if true, tree is left heavy *)
      if ((get_height l) - (get_height r)) = 2 then        
        match l with
	| Leaf -> failwith "l error"
	| Branch(_,lv,ll,lr) ->
          if ( (get_height ll) - (get_height lr) ) > 0 
          then
            (* LL imbalance *) 
            let rh' = max (get_height lr) (get_height r) + 1 in
            let h' = max rh' (get_height ll) + 1 in
            Branch(h', lv, ll, Branch(rh',v,lr,r))
	  else 
            (* LR imbalance *)
            match lr with
	    | Leaf -> failwith "lr error"
	    | Branch(_,lrv,lrl,lrr) ->
               let lh' = max (get_height ll) (get_height lrl) + 1 in
               let rh' = max (get_height lrr) (get_height r) + 1 in
               let h' = (max lh' rh') + 1 in
               Branch(h', lrv, Branch(lh',lv,ll,lrl), Branch(rh',v,lrr,r)) 
      (* Determine if tree is right heavy *)
      else if ((get_height l) - (get_height r)) = ~-2 then 
        match r with
	| Leaf -> failwith "r error"
	| Branch(_,rv,rl,rr) ->
          if (get_balance r) > 0
	  then   
            match rl with
	    | Leaf -> failwith "rl error"
	    | Branch(_,rlv,rll,rlr) ->
              let lh' = (max (get_height l) (get_height rll)) + 1 in
              let rh' = (max (get_height rlr) (get_height rr)) + 1 in
              let h' = (max lh' rh') + 1 in
              Branch(h', rlv, Branch(lh',v,l,rll), Branch(rh',rv,rlr,rr))
	  else (* RR *)
            match rr with
	    | Leaf -> failwith "rr error"
	    | Branch(_,_,_,_) ->
               let lh' = (max (get_height l) (get_height rl)) + 1 in
               let h' = (max lh' (get_height rr)) + 1 in
               Branch(h', rv, Branch(lh',v,l,rl), rr)
      else t
  
 

  (* inserts an element into the tree *)
  let rec insert (e : elt) (t : tree) : tree =
    match t with
    | Leaf -> Branch (1, e, Leaf, Leaf)
    | Branch(_,v,l,r) ->
       match (C.compare e v) with
       | EQUAL -> t
       | LESS ->
          let temp = insert e l in
          let h' = (max (get_height temp) (get_height r)) + 1 in
          balance (Branch (h', v, temp, r))
       | GREATER ->
          let temp = insert e r in 
          let h' = (max (get_height temp) (get_height r)) + 1 in
          balance (Branch (h', v, l, temp)) 
 

  let rec pull_max (t : tree) : elt * tree =
    match t with
    | Leaf -> failwith "empty tree"
    | Branch(_,v,Leaf,Leaf) -> (v, Leaf)
    | Branch(_,v,l,Leaf) -> (v, l)
    | Branch(h,v,l,r) -> 
      let v', t' = pull_max r in (v', (balance (Branch(h,v,l,t'))))

 
  let rec delete (e : elt) (t : tree) : tree =
    match t with
    | Leaf -> Leaf
    | Branch(_,v,l,r) ->
      match C.compare e v with
      | LESS -> 
        let temp = delete e l in
        let h' = (max (get_height temp) (get_height r)) + 1 in
        balance (Branch(h',v,temp,r))
      | GREATER -> 
        let temp = delete e r in
        let h' = (max (get_height l) (get_height temp)) + 1 in
        balance (Branch(h',v,l,temp))
      | EQUAL ->
        match l, r with
	| Leaf, Leaf -> Leaf
	| Leaf, _ -> r
	| _, Leaf -> l
	| _,_ ->
          let v',l' = pull_max l in
          let h' = (max (get_height l') (get_height r)) + 1 in
          balance(Branch(h',v',l',r))

  let rec search (e : elt) (t: tree) : bool =
  match t with
  | Leaf -> false
  | Branch(_,v,l,r) -> 
      (match C.compare e v with
       | LESS -> search e l
       | GREATER -> search e r
       | EQUAL -> true)
end




(* TEST CASES FOR AVL TREES *)

module A = AVLTree(IntCompare);;

let t1 = A.empty;;
let t2 = A.insert 55 t1;;
let t3 = A.insert 99 t2;;
let t4 = A.insert 40 t3;;
let t5 = A.insert 120 t4
let t6 = A.insert 35 t5;;
let t7 = A.insert 12 t6;;
let t8 = A.insert 88 t7;;
let t9 = A.insert 23 t8;;


assert(t1 = A.Leaf);;
assert(t2 = A.Branch (1, 55, A.Leaf, A.Leaf));;
assert(t3 = A.Branch (2, 55, A.Leaf, A.Branch (1, 99, A.Leaf, A.Leaf)));;
assert(t4 = A.Branch (2, 55, A.Branch (1, 40, A.Leaf, A.Leaf),
 A.Branch (1, 99, A.Leaf, A.Leaf)));;
assert(t5 = A.Branch (3, 55, A.Branch (1, 40, A.Leaf, A.Leaf),
 A.Branch (2, 99, A.Leaf, A.Branch (1, 120, A.Leaf, A.Leaf))));;
assert(t6 = A.Branch (3, 55, A.Branch (2, 40, A.Branch (1, 35, A.Leaf, A.Leaf), A.Leaf),
 A.Branch (2, 99, A.Leaf, A.Branch (1, 120, A.Leaf, A.Leaf))))
assert(t7 = A.Branch (3, 55,
 A.Branch (2, 35, A.Branch (1, 12, A.Leaf, A.Leaf),
  A.Branch (1, 40, A.Leaf, A.Leaf)),
 A.Branch (2, 99, A.Leaf, A.Branch (1, 120, A.Leaf, A.Leaf))));;
assert(t8 = A.Branch (3, 55,
 A.Branch (2, 35, A.Branch (1, 12, A.Leaf, A.Leaf),
  A.Branch (1, 40, A.Leaf, A.Leaf)),
 A.Branch (2, 99, A.Branch (1, 88, A.Leaf, A.Leaf),
  A.Branch (1, 120, A.Leaf, A.Leaf))));;
assert(t9 = A.Branch (4, 55,
 A.Branch (3, 35, A.Branch (2, 12, A.Leaf, A.Branch (1, 23, A.Leaf, A.Leaf)),
  A.Branch (1, 40, A.Leaf, A.Leaf)),
 A.Branch (2, 99, A.Branch (1, 88, A.Leaf, A.Leaf),
  A.Branch (1, 120, A.Leaf, A.Leaf))));;
assert(A.delete 9 t9 = A.Branch (4, 55,
 A.Branch (3, 35, A.Branch (2, 12, A.Leaf, A.Branch (1, 23, A.Leaf, A.Leaf)),
  A.Branch (1, 40, A.Leaf, A.Leaf)),
 A.Branch (2, 99, A.Branch (1, 88, A.Leaf, A.Leaf),
  A.Branch (1, 120, A.Leaf, A.Leaf))));;
assert(A.delete 10 A.Leaf = A.Leaf);;
assert(A.delete 88 t9 = A.Branch (4, 55,
 A.Branch (3, 35, A.Branch (2, 12, A.Leaf, A.Branch (1, 23, A.Leaf, A.Leaf)),
  A.Branch (1, 40, A.Leaf, A.Leaf)),
 A.Branch (2, 99, A.Leaf, A.Branch (1, 120, A.Leaf, A.Leaf))));;
assert(A.search 88 t9 = true);;
assert(A.search 55 t9 = true);;
assert(A.search 0 t9 = false);;
