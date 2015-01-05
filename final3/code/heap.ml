open Order
exception EmptyTree

module BHEAP(C : COMPARABLE)  =
  struct  
    type elt = C.t
    type balance = Odd | Even
    type dir = Left | Right | Neither
    type tree = | TwoBranch of balance * tree * elt * tree 
                | OneBranch of elt * elt
	        | Leaf of elt
                | Emptyt
    
    let empty = Emptyt
    
    let rec insert (x: elt) (t: tree) : tree =
      match t with
      | Emptyt -> Leaf(x) 
      | Leaf (e1)-> (match C.compare x e1 with
                     | EQUAL -> t
                     | GREATER -> OneBranch (e1, x)
		     | LESS -> OneBranch (x, e1))
    
      | OneBranch (e1, e2) ->   (* error... probar insert 5, 7, luego 3 *)
                   (match C.compare x e1 with
		    | EQUAL -> t
		    | GREATER -> TwoBranch(Even, Leaf (e2), e1, Leaf (x)) 
		    | LESS -> TwoBranch(Even, Leaf (e2), x, Leaf (e1)))
    

    | TwoBranch (Even, t1, e1, t2)->
                   (match C.compare x e1 with
		    | EQUAL -> t
		    | GREATER ->  TwoBranch(Odd, insert x t1, e1, t2)
		    | LESS -> TwoBranch(Odd, insert e1 t1, x, t2))
                   


    | TwoBranch (Odd, t1, e1, t2) -> 
                   (match C.compare x e1 with
		    | EQUAL -> t
		    | GREATER ->  TwoBranch(Even, t1, e1, insert x t2)
		    | LESS -> TwoBranch(Even, t1, x, insert e1 t2))
    
    (* MAYBE THIS SHOULD BE FOR BALANCED TREES *)
    let rec search (x : elt) (t : tree) =
      match t with
      | Leaf e ->
        (match C.compare x e with EQUAL -> true | _ -> false)
      | OneBranch (e1,e2) ->
        ((C.compare x e1) = EQUAL) || ((C.compare x e2) = EQUAL)
      | TwoBranch (_,l,e,r) -> 
        (match C.compare x e with
	 | EQUAL -> true
	 | _ -> (search x l) || (search x r))
      | _ -> raise EmptyTree
        

    let get_top (t: tree) : elt =
      match t with
      | Emptyt -> raise EmptyTree
      | Leaf (e)
      | OneBranch (e,_) 
      | TwoBranch (_,_,e,_) -> e 

    let compare3 (e1 : elt) (e2 : elt) (e3 : elt) : dir =
      match C.compare e2 e3 with
      | GREATER -> 
        (match C.compare e1 e3 with
	 | GREATER -> Right
	 | _ -> Neither)
      | _ -> 
        (match C.compare e1 e2 with
	 | GREATER -> Left
	 | _ -> Neither)

    
    let swap (e : elt) (t : tree) =
      match t with
      | Emptyt -> raise EmptyTree
      | Leaf _ -> Leaf e
      | OneBranch (_,e1) -> OneBranch (e,e1)
      | TwoBranch (b,t1,_,t2) -> TwoBranch (b,t1,e,t2)

    let rec fix (t : tree) : tree =
      match t with
      | Emptyt -> raise EmptyTree
      | Leaf _ -> t
      | OneBranch (e1,e2) -> 
        (match C.compare e1 e2 with
	 | GREATER -> OneBranch (e2, e1)
	 | _ -> t)
      | TwoBranch (b,t1,e,t2) ->
          let (top1,top2) = (get_top t1,get_top t2) in
          (match compare3 e top1 top2 with
	   | Neither -> t
	   | Left -> TwoBranch(b, fix (swap e t1), top1, t2)
	   | Right -> TwoBranch(b, t1, top2, fix (swap e t2)))


    let rec get_last (t : tree) : elt * tree =
      match t with
      | Emptyt -> raise EmptyTree
      | Leaf e -> e, Emptyt
      | OneBranch (e1,e2) -> e2, (Leaf e1)
      | TwoBranch (Even, t1, e, t2) ->
        let (last, q2') = get_last t2 in
        (match q2' with
	 | Emptyt -> last, (OneBranch(e, get_top t1))
	 | _ as t2' -> last, (TwoBranch(Odd, t1,e,t2')))
      | TwoBranch (Odd, t1, e, t2) ->
        let (last, q1') = get_last t1 in
        last, (TwoBranch(Even, q1', e, t2))



    let take (t : tree) : elt * tree = 
      match t with
      | Emptyt -> raise EmptyTree 
      | Leaf e -> e, Emptyt
      | OneBranch(e1,e2) -> e1, (Leaf e2)
      | TwoBranch(Even, t1,e,t2) ->
        let (last, q2') = get_last t2 in
        (match q2' with
	 | Emptyt -> (e, (fix (OneBranch (last, get_top t1))))
	 | _ as t2' -> (e, (fix (TwoBranch (Odd, t1, last, t2')))))
      | TwoBranch(Odd,t1,e,t2) ->
         let last, q1' = get_last t1 in
        (match q1' with
	 | Emptyt -> failwith "invariant broken"
	 | t1' -> e, (fix (TwoBranch (Even, t1', last, t2))))

end
