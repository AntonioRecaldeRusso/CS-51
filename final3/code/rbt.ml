(*ATENTION in order to make this Red Black Tree several internet sources were consulted 
 * so the functions here are Large based on what we could f ind in:
 * http://www.cs.cornell.edu/courses/cs3110/2009sp/lectures/lec11.html
 * https://github.com/bmeurer/ocaml-rbtrees
 * https://github.com/aduraj/ocaml-data-structures
 * http://rosettacode.org/wiki/Pattern_matching
 * http://benediktmeurer.de/2011/10/16/red-black-trees-for-ocaml/
 * https://www.lri.fr/~filliatr/fsets/index.en.html
 * http://www.cs.princeton.edu/~appel/papers/redblack.pdf
 * http://stackoverflow.com/questions/3176863/concatenating-red-black-trees
 * http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/a6.php
 * http://www.cs.cmu.edu/~rwh/introsml/techniques/repinv.htm
 * http://www.cs.utexas.edu/users/kaufmann/itp-2010/session8/lammich-ITP_2010_ICF.pdf
 * http://www.cs.uiuc.edu/class/su07/cs421/project/proposals/tjbrenna-pjoseph2/proposal.txt
 * http://docs.happycoders.org/orgadoc/dev/ocaml/ocaml.pdf
 * http://cs51.seas.harvard.edu/docs/lectures/2013/1-overview-4up.pdf
 * https://www.youtube.com/watch?v=vDHFF4wjWYU
 * https://www.lri.fr/~filliatr/ftp/ocaml/ds/rbset.ml.html  
 * and we adapted this to fit our needs *)



open Order

module RBT (C : COMPARABLE) =
  struct

    type elt = C.t
    type color = Black 
               | Red
  
    type tree = Leaf 
              | Tree of color * tree * elt * tree 
           


    let empty : tree = Leaf

    let is_empty (t : tree) : bool = (t = Leaf)
  

    let rec search (e : elt) (t : tree) : bool = 
      match t with
      | Leaf -> false
      | Tree (_, left, e2, right) ->  
          match C.compare e e2 with
	  | EQUAL -> true
          | LESS -> search e left
	  | GREATER -> search e right
   

  (* To insert an element on a tree*)

    let left_bal (t: tree) : tree = 
      match t with
      | Leaf -> Leaf
      | Tree (_, Tree (Red, (Tree (Red, left1, e1, right1)), e2, right2), e3, right3) ->
          Tree (Red, (Tree (Black, left1, e1, right1)), e2, 
          (Tree (Black, right2, e3, right3)))
      | Tree (_, (Tree (Red, left1, e1, Tree (Red, left2, e2, right1))), e3, right2) ->
          Tree (Red, Tree (Black, left1, e1, right1), e2, 
          Tree (Black, left2, e3, right2))
      | Tree (_, left, e, right) ->
          Tree (Black, left, e, right)


    let right_bal (t : tree) : tree = 
      match t with
      | Leaf -> Leaf
      | Tree (_, left1, e1, Tree (Red, (Tree (Red ,left2,e2,right1)), e3, right2)) ->
          Tree (Red, (Tree (Black ,left1,e1,left2)), e2, Tree(Black ,right1,e3,right2))
      | Tree (_, left1, e1, Tree (Red ,left2, e2, Tree (Red ,left3,e3,right1))) ->
          Tree (Red, (Tree(Black ,left1,e1,left2)), e2, Tree (Black ,left3,e3,right1))
      | Tree(_,left,e,right) ->
          Tree (Black,left,e,right)

    let first_black (t: tree) : tree =
    match t with
    | Leaf -> Leaf
    | Tree(_, left, value, right) ->  Tree (Black, left, value, right)

  
    let first_black_bool (t: tree) : tree * bool =
      match t with
      | Tree (Red, left, e, right) -> Tree(Black, left, e, right), false 
      | othert -> othert, true


    let rec inserted (e: elt) (t: tree)= 
      match t with
      | Leaf ->
          Tree (Red , Leaf, e, Leaf)
      | Tree (Red ,left, e2, right) ->
          (match  C.compare e e2 with
          | LESS ->  Tree (Red ,(inserted e left), e2, right)
          | EQUAL -> Tree (Red ,left, e2, right)
          | GREATER ->  Tree (Red, left, e2, (inserted e right)))
          
      | Tree (Black ,left, e2, right)  ->
          (match  C.compare e e2 with
          | LESS -> left_bal (Tree(Black, (inserted e left), e2, right))
	  | EQUAL -> Tree (Black ,left, e2, right) 
          | GREATER ->  right_bal (Tree(Black, left, e2, (inserted e right))))
         
    
    let insert (e : elt) (t: tree) =
      first_black (inserted e t)

  (* To Remove a element from a tree *)

  (* repair_left repares the tree after the deletion deletion of 
    an element wich makes the height of the tree change  *)

    let repair_left (t : tree) : tree * bool = 
      match t with
      | Tree (Red, (Tree (Black, left1, e1, right1)), e2, right2) ->
          (left_bal (Tree(Black, Tree (Red, left1, e1, right1), e2, right2))), false
      | Tree (Black, (Tree (Black, left1, e1, right1)), e2, right2) ->
          left_bal (Tree (Black , (Tree(Red ,left1, e1, right1)), e2, right2)), true
      | Tree (Black,(Tree(Red, left1, e1, Tree(Black, left2, e2, right1))), e3, right2) ->
          (Tree(Black, left1, e1, left_bal (Tree(Black , 
             Tree (Red ,left2, e2, right1), e3, right2)))), false
      | _ -> failwith "Unexpected"

   (* repair_right repares the tree after the deletion deletion of
      an element wich makes the height of the tree change  *)

    let repair_right (t: tree) : tree * bool = 
      match t with 
      | Tree (Red, left1, e1, Tree (Black ,left2, e2, right1)) ->
        right_bal (Tree(Black, left1, e1, (Tree (Red, left2, e2, right1)))) ,false
      | Tree (Black, left1, e1, Tree (Black, left2, e2, right1)) ->
        right_bal (Tree (Black, left1, e1, (Tree (Red, left2, e2, right1)))), true
      | Tree(Black, left1, e1, (Tree(Red, Tree(Black ,left2, e2, right1), e3, right2))) ->
        Tree (Black, (right_bal (Tree (Black, left1, e1, 
         (Tree (Red ,left2, e2, right1))))), e3, right2), false
      | _ -> failwith "Unexpected"


  (* take_min extracts the minimum froma a tree t and gives back the new 
   * tree * the element extracted * and a bool
     indicating if the height of the blacks  has changed  *)

    let rec take_min (t: tree) : tree * elt * bool = 
      match t with
      | Leaf -> failwith "Unexpected"
      | Tree (Black, Leaf, e, Leaf) ->
          Leaf, e, true
      | Tree (Black, Leaf, e, Tree (Red, left, e2, right)) ->
          Tree (Black ,left, e2, right), e, false
      | Tree (Black, Leaf, _, Tree (Black, _, _, _)) -> failwith "Unexpected"
      | Tree (Red , Leaf, val1, right) ->
          right, val1, false
      | Tree (Black , left, val1, right) ->
          let left1, min , change = take_min left in
          let tree1 = Tree (Black ,left1, val1, right) in
          if change then
            let tree1 ,change1 = repair_right tree1 in tree1,min,change1
          else
            tree1, min, false
      | Tree (Red ,left, val1, right) ->
          let left1,min,change = take_min left in
          let tree1 = Tree (Red ,left1, val1, right) in
          if change then
            let tree1,change1 = repair_right tree1 in tree1,min,change1
          else
            tree1, min, false

  

  (* to remove an element e from a tree t *)

    let remove (e : elt) (t : tree) : tree =
      let rec deleting t = 
        match t with
        | Leaf -> Leaf, false
        | Tree (Black ,left, e1, right) ->
            let c = C.compare e e1 in
            if (c = LESS) then
              let left1, change = deleting left in
              let t = Tree (Black ,left1, e1, right) in
            if change then repair_right t else t, false
            else if (c = GREATER) then
              let right1 , change = deleting right in
              let tree1 = Tree (Black ,left, e1, right1) in
            if change then repair_left tree1 else tree1, false
            else 
              (match right with
                 | Leaf ->
                     first_black_bool left
                 | _ ->
                   let right1,min,change = take_min right in
                   let tree1 = Tree (Black ,left, min, right1) in
                   if change then repair_left tree1 else tree1, false)
        | Tree (Red ,left, e1, right) ->
          let c = C.compare e e1 in
            if (c = LESS) then
              let left1,change = deleting left in
              let tree1 = Tree (Red ,left1, e1, right) in
            if change then repair_right tree1 else tree1, false
            else if (c = GREATER) then
              let right1,change = deleting right in
              let tree1 = Tree (Red ,left, e1, right1) in
            if change then repair_left tree1 else tree1, false
            else 
              (match right with
                 | Leaf -> left, false
                 | _ ->
                   let right1,min,change = take_min right in
                   let tree1 = Tree (Red ,left, min, right1) in
                   if change then repair_left tree1 else tree1, false)
      in
      let tr,_ = deleting t in tr

    let delete (e : elt) (t : tree) : tree =
      first_black (remove e t)
  
    
    let str_of_color (c : color) : string =
      match c with
      | Black -> "Black"
      | Red -> "Red" 

    let rec printbdf (t :tree) : unit =
    match t with
    | Leaf -> print_string "Leaf "
    | Tree (color, left, value, right) ->
      printbdf left; print_string ((str_of_color color) ^ " "); print_string (C.to_string value) ; print_string " "; printbdf right
    
end



