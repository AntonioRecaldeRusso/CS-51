(* 
 * ATTENTION:
 * This 2-3 tree implementation is LARGELY based on the BTDict module
 * pset5 "moogle". We readjusted it to fit it in our tests along with other
 * data structures
 *
 *)


open Core.Std
open Order

module TwoThreeTree(C : COMPARABLE) =
struct
  type key = C.t
  type dict =
    | Leaf
    | Two of dict * key * dict
    | Three of dict * key * dict * key * dict
  type kicked =
    | Up of dict * key * dict
    | Done of dict

  type hole =
    | Hole of key option * dict
    | Absorbed of key option * dict

  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  let empty : dict = Leaf

  let insert_upward_two w (w_left: dict) (w_right: dict)
      x (x_other: dict) : kicked =
    match C.compare w x with
    | LESS -> Done (Three(w_left, w, w_right, x, x_other))
    | GREATER -> Done (Three(x_other, x, w_left, w, w_right))
    | EQUAL -> Done (Two(w_left, x, w_right))

   let insert_upward_three w (w_left: dict) (w_right: dict)
      x y (other_left: dict) (other_right: dict) : kicked =
    match C.compare w x with
    | LESS -> Up ((Two(w_left,w,w_right)), x, (Two(other_left,y,other_right)))
    | _ -> match C.compare w y with
	   | LESS -> 
               Up ((Two(other_left,x,w_left)), w, (Two(w_right,y,other_right)))
	   | _ -> 
               Up ((Two(other_left,x,other_right)), y, (Two(w_left,w,w_right))) 

   let rec insert_downward (d: dict) (k: key) : kicked =
    match d with
      | Leaf -> Done (Two(Leaf, k, Leaf))
      | Two(left,n,right) -> insert_downward_two k n left right
      | Three(left,n1,middle,n2,right) -> 
          insert_downward_three k n1 n2 left middle right

  and insert_downward_two (k) (k1)
      (left: dict) (right: dict) : kicked = 
      if ((C.compare k k1)= EQUAL) then Done (Two(left,k1,right)) else
      match left with
      | Leaf -> insert_upward_two k Leaf Leaf k1 Leaf
      | _ -> (match C.compare k k1 with
	      | LESS -> (match insert_downward left k with
			 | Up (w_left, w, w_right) -> 
                             insert_upward_two w w_left w_right k right
			 | Done d -> Done (Two(d, k1, right)))
	      | _ -> (match insert_downward right k with
		      | Up (w_left, w, w_right) -> 
                          insert_upward_two w w_left w_right k1 left
		      | Done d -> Done (Two(left, k1, d))))
	      
      

  and insert_downward_three k k1 k2
      (left: dict) (middle: dict) (right: dict) : kicked =
      if ((C.compare k k1) = EQUAL) 
      then Done (Three(left,k1,middle,k2,right))
      else 
        if ((C.compare k k2)= EQUAL) 
        then Done (Three(left,k1,middle,k2,right)) 
        else
          match left with
          | Leaf -> insert_upward_three k Leaf Leaf k1 k2 Leaf Leaf
          | _ -> 
             (match C.compare k k1, C.compare k k2 with
              | LESS, _ -> (match insert_downward left k with
                            | Up (w_left, w, w_right) -> insert_upward_three 
                                w w_left w_right k1 k2 middle right
                            | Done d -> 
                                Done (Three(d,k1, middle,k2, right)))                       
              | _, LESS -> (match insert_downward middle k with
                            | Up (w_left, w, w_right) -> insert_upward_three 
                                w w_left w_right k1 k2 left right
                            | Done d -> 
                                Done (Three(left, k1, d, k2, right)))
              | _, _ -> (match insert_downward right k with
                         | Up (w_left, w, w_right) -> insert_upward_three 
                             w w_left w_right k1 k2 left middle
                         | Done d -> 
                             Done (Three(left, k1, middle, k2, d))))
     

  let insert (d: dict) (k: key) : dict =
    match insert_downward d k with
      | Up(l,x,r) -> Two(l,x,r)
      | Done x -> x

  let remove_upward_two n (rem: key option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem, Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) -> Absorbed(rem, Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))



  let remove_upward_three n1 n2 (rem: key option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem, Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem, Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e -> 
          Absorbed(rem, Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e -> 
          Absorbed(rem, Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) -> 
          Absorbed(rem, Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e -> 
          Absorbed(rem, Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,k1,Leaf) ->
        (match C.compare k k1 with
          | EQUAL -> Hole(Some k1,Leaf)
          | LESS | GREATER -> Absorbed(None,d)
        )
      | Three(Leaf,k1,Leaf,k2,Leaf) ->
        (match C.compare k k1, C.compare k k2 with
          | EQUAL, _ -> Absorbed(Some k1,Two(Leaf,k2,Leaf))
          | _, EQUAL -> Absorbed(Some k2,Two(Leaf,k1,Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  and remove_downward_two (k: key) (k1)
      (left: dict) (right: dict) : hole =
    match C.compare k k1 with
      | EQUAL ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | LESS ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two k1 rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,k1,right))
        )
      | GREATER ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two k1 rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,k1,t))
        )

  and remove_downward_three (k: key) (k1) (k2)
      (left: dict) (middle: dict) (right: dict) : hole =
    match C.compare k k1, C.compare k k2 with
      | EQUAL, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,k2,right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n k2 None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,k1,right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,k2,right))
        )
      | _ , EQUAL ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,k1,middle))
          | Hole(Some n,new_right) ->
            remove_upward_three k1 n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,k1,middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,k1,middle,n,new_right))
        )
      | LESS, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three k1 k2 rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,k1,middle,k2,right))
        )
      | _, GREATER ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three k1 k2 rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,k1,middle,k2,t))
        )
      | GREATER, LESS ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three k1 k2 rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,k1,t,k2,right))
        )

  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  let delete (d: dict) (k : key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  let rec search (d: dict) (k: key) : bool =
    match d with
    | Leaf -> false
    | Two (left, key, right) -> 
      (match C.compare k key with
       | EQUAL -> true
       | LESS  -> search left k
       | GREATER -> search right k)
    | Three (left, l_key, middle, r_key, right) ->
      (match (C.compare k l_key) with
       | EQUAL -> true
       | LESS -> search left k
       | GREATER -> (match C.compare k r_key with
		     | EQUAL -> true
		     | LESS -> search middle k
		     | GREATER -> search right k))

end
