open Core.Std
open Util
open CrawlerServices
open Order
open Nodescore
open Graph


(* Dictionaries mapping links to their ranks. Higher is better. *)
module RankDict = Dict.Make(
  struct
    type key = link
    type value = float
    let compare = link_compare
    let string_of_key = string_of_link
    let string_of_value = Float.to_string
    let gen_key () = {host=""; port=0; path=""}
    let gen_key_gt _ () = gen_key ()
    let gen_key_lt _ () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ () = None
    let gen_value () = 0.0
    let gen_pair () = (gen_key(),gen_value())
  end)

module PageSet = Myset.Make(
  struct
    type t = page
    let compare = (fun a b -> link_compare (a.url) (b.url))
    let string_of_t = string_of_page
    let gen () = {url={host=""; port=0; path=""}; links=[]; words=[]}
    let gen_lt _ () = gen ()
    let gen_gt _ () = gen ()
    let gen_random () = gen ()
    let gen_between _ _ () = None
  end)

module LinkSet = Myset.Make(
  struct
    type t = link
    let compare = link_compare
    let string_of_t = string_of_link
    let gen () = {host=""; port=0; path=""}
    let gen_lt _ () = gen ()
    let gen_gt _ () = gen ()
    let gen_random () = gen ()
    let gen_between _ _ () = None
  end)

module PageGraph = Graph (
  struct
    type node = link
    let compare = link_compare
    let string_of_node = string_of_link
    let gen () = {host=""; port=0; path=""}
  end)

module PageScore = NodeScore (
  struct
    type node = link
    let string_of_node = string_of_link
    let compare = link_compare
    let gen () = {host=""; port=0; path=""}
  end)

(* Given a bunch of pages, convert them to a graph *)
let graph_of_pages (pages : PageSet.set) : PageGraph.graph =
  (* Only want graph nodes for pages we actually crawled *)
  let crawled_links =
    PageSet.fold (fun page s -> LinkSet.insert page.url s)
      LinkSet.empty pages
  in
  let add_links page graph =
    let add_link g dst =
      if LinkSet.member crawled_links dst then
        PageGraph.add_edge g page.url dst
      else g
    in
      List.fold_left page.links ~f:add_link ~init:graph
  in
    PageSet.fold add_links PageGraph.empty pages

(* The rest of the world wants a RankDict, not a NodeScore. *)

let dict_of_ns (ns : PageScore.node_score_map) : RankDict.dict =
  PageScore.fold (fun node score r -> RankDict.insert r node score)
    RankDict.empty ns

(* A type for modules that can compute nodescores from graphs *)
module type RANKER =
sig
  module G: GRAPH
  module NS: NODE_SCORE
  val rank : G.graph -> NS.node_score_map
end


(* Each node's rank is equal to the number of pages that link to it. *)
module InDegreeRanker  (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) :
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA
  let rank (g : G.graph) =
    let add_node_edges ns node =
      let neighbors = match G.neighbors g node with
        | None -> []
        | Some xs -> xs
      in
        List.fold_left neighbors ~f:(fun ns' neighbor -> NS.add_score ns' neighbor 1.0) ~init:ns
    in
    let nodes = (G.nodes g) in
      List.fold_left nodes ~f:add_node_edges ~init:(NS.zero_node_score_map nodes)
end



(*****************************************************************)
(* KARMA                                                         *)
(* Random Walk Ranker                                            *)
(*****************************************************************)


module type WALK_PARAMS =
sig
  (* Should we randomly jump somewhere else occasionally?
    if no, this should be None.  Else it should be the probability of
    jumping on each step *)
  val do_random_jumps : float option
  (* How far must sisyphus walk? *)
  val num_steps : int
end

module RandomWalkRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N)
  (P : WALK_PARAMS) :
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA
  
  (* TODO - fill this in*)
  (*
val rank : G.graph -> NS.node_score_map
let rank (g : G.graph) = ???
let rec walk (num_steps) (next node) (node score) =
if steps 0 -> node score
else pick next node randomly from adjacent nodes
call it new_node add 1 to next node in nodescore -> num_steps -1 new_node NewNodeScore

grapg.ml -> G.get_random_node, G.neighbours
nodescore.ml -> NS.zero_score_node_map, NS.add_score, NS.normalize  *)


  let num_steps = P.num_steps
  let alpha = P.do_random_jumps
  
  (* returns a random element from a list *)
  let random_from_list lst =
    let len = List.length lst in
    let random_int = if len = 0 then 0 else Random.int len in
    List.nth lst random_int

  (* returns a random neighbor from a list of neighbors *)
  let random_neighbor list_of_neighbor =
    match list_of_neighbor with
    | None -> None
    | Some xs -> Some (random_from_list xs)

  (* deoptionalizes *)
  let deoptionalize n =
    match n with
    | None -> failwith "No neighbors"
    | Some n -> n

  (* Walks Sisyphus *)
  let rec walk (g: G.graph) (num_steps: int) (node) 
        (ns_map: NS.node_score_map) : NS.node_score_map =
    if G.is_empty g then ns_map else
    match num_steps with
    | 0 -> ns_map 
    | _ -> let a =
             match alpha with
	     | None -> 0.0
	     | Some a -> a in
             let n = random_neighbor (G.neighbors g node) in
	       let new_node = 
                 if ((Random.float 1.0 < a) || (n = None || n = Some None)) 
                 then G.get_random_node g
                 else (deoptionalize n) in
               let ns_map' = NS.add_score ns_map (deoptionalize new_node) 1.0 in
               let new_steps = num_steps - 1 in
               walk g new_steps (deoptionalize new_node) ns_map'
	      
  (* returns a node_score_map after walking though a graph *)              			    
  let rank (g: G.graph) : NS.node_score_map =
    let nodes = (G.nodes g) in
    let node_score = NS.zero_node_score_map nodes  in
    let random_node = G.get_random_node g in  
    NS.normalize (walk g num_steps (deoptionalize random_node) node_score)
 
end



(*****************************************************************)
(* KARMA                                                         *)
(* Quantum Ranker                                                *)
(*****************************************************************)

(*
module type QUANTUM_PARAMS =
sig
  (* What fraction of each node's score should be uniformly distributed
     to the graph as a whole? *)
  val alpha : float
  (* How many rounds? *)
  val num_steps : int

  (* Print stuff? *)
  val debug : bool
end

module QuantumRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N)
  (P : QUANTUM_PARAMS) :
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA

  (* TODO - fill this in *)
end
*)


(*******************  TESTS BELOW  *******************)

module TestInDegreeRanker =
struct
  module G = NamedGraph
  let g = G.add_edge G.empty "a" "b";;
  let g2 = G.add_edge g "a" "c";;

  module NS = NodeScore (struct
                           type node = string
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = InDegreeRanker (G) (NS);;
  let ns = Ranker.rank g2;;
  (* let _ = Printf.printf "NS: %s\n" (NS.string_of_node_score_map ns) ;; *)
  assert ((NS.get_score ns "a") = Some 0.0);;
  assert ((NS.get_score ns "b") = Some 1.0);;
  assert ((NS.get_score ns "c") = Some 1.0);;
  assert ((NS.get_score ns "d") = None);;

  let g3 = G.add_edge g2 "b" "c";;
  let ns2 = Ranker.rank g3;;
  assert ((NS.get_score ns2 "a") = Some 0.0);;
  assert ((NS.get_score ns2 "b") = Some 1.0);;
  assert ((NS.get_score ns2 "c") = Some 2.0);;

end


module TestRandomWalkRanker =
struct
  module G = NamedGraph
  let g = G.from_edges [("a","b") ;
                        ("b","c") ;
                        ("c","d") ;
                        ("d","e") ;
                        ("e","f") ;
                        ("a","g") ;
                        ("g","a")]

  module NS = NodeScore (struct
                           type node = string
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = RandomWalkRanker (G) (NS)
    (struct
       let do_random_jumps = None
       let num_steps = 1000
     end)

  let ns = Ranker.rank g
  let _ = Printf.printf "Testing RandomWalkRanker:\n NS: %s\n"
    (NS.string_of_node_score_map ns)

(* That's the problem with randomness -- hard to test *)
end



(*
module TestQuantumRanker =
struct
  module G = NamedGraph
  let g = G.from_edges [("a","b") ;
                        ("a","c") ;
                        ("b","c") ;
                        ("c","a")]

  module NS = NodeScore (struct
                           type node = string
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = QuantumRanker (G) (NS)
    (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)

  let ns = Ranker.rank g
  let _ = Printf.printf "Testing Quantum ranker:\n %s\n"
    (NS.string_of_node_score_map ns)

(* That's the problem with randomness -- hard to test *)
end
*)







