open Core.Std
open Util
open CrawlerServices
open Order
open Pagerank


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
<<<<<<< HEAD
(*  = InDegreeRanker (PageGraph) (PageScore) *)
  
=======
  = InDegreeRanker (PageGraph) (PageScore)
  (*
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
<<<<<<< HEAD
  
=======
  *)
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
<<<<<<< HEAD
    let gen_key_gt _ () = gen_key ()
    let gen_key_lt _ () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ () = None
=======
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
<<<<<<< HEAD


(* This is a helper function for "crawl." It inserts elements of
 * a list "lst" into a set "nset"                                    *)
let insert_list (nset: LinkSet.set) (lst: link list) : LinkSet.set =
  List.fold_left lst ~f:(fun r k -> LinkSet.insert k r) ~init:nset

(* Inserts words in list "wl" into WordDict "d" *)
let rec insert_words  (l : link) (d : WordDict.dict) 
    (wl : string list) : WordDict.dict =
  match wl with
  | [] -> d
  | w :: tl ->
    let d' =
      match WordDict.lookup d w with
      | Some x -> WordDict.insert d w (LinkSet.insert l x)
      | None -> WordDict.insert d w (LinkSet.singleton l) in
    insert_words l d' tl

(* Crawls though pages, builds dictionary in the process  *)
let rec crawl (n : int) (frontier : LinkSet.set)
    (visited : LinkSet.set) (d : WordDict.dict) : WordDict.dict =
  match n, LinkSet.choose frontier with
  | 0, _ | _, None -> d
  | _,Some (elem, nset) ->  
       (match CrawlerServices.get_page elem with
        | None -> crawl n nset visited d
        | Some p ->  
         (match LinkSet.member visited elem with
	 | true -> crawl n nset visited d
         | false -> crawl (n-1) (insert_list nset p.links) (LinkSet.insert elem visited) 
                          (insert_words p.url d p.words)))
=======
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  WordDict.empty
>>>>>>> 4e4637b59e7bde94ce84d599a738643b7d84e067
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
