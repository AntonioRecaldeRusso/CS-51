open Core.Std
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let next_gold_id = ref 0
let get_next_gold_id () =
  let p = !next_gold_id in incr next_gold_id ; p

(* ### Part 3 Actions ### *)
let max_gold = 5
let produce_gold_probability = 50
let expand_probability = 2000
let forfeit_gold_probability = 3

(* ### Part 4 Aging ### *)
let town_lifetime = 2000

(** Towns produce gold.  They will also eventually die if they are not cross
    pollenated. *)
class town p gold_id : Ageable.ageable_t =
object (self)
inherit CarbonBased.carbon_based p None (World.rand town_lifetime) town_lifetime

  (******************************)
  (***** Instance Variables *****)
  (******************************)
  
  val mutable town_gold = World.rand max_gold
  
  (* ### TODO: Part 3 Actions ### *)
  
  (***********************)
  (***** Initializer *****)
  (***********************)
  initializer
    self#register_handler World.action_event self#do_action 
  
  (* ### TODO: Part 3 Actions ### *)
  
  (**************************)
  (***** Event Handlers *****)
  (**************************)
  
  (* ### TODO: Part 3 Actions ### *)
  method private produce_gold = 
   if (town_gold < max_gold) 
   then with_inv_probability World.rand produce_gold_probability 
     (fun () -> town_gold <- (town_gold + 1)) 
   else ()
  
  method private expand = 
    with_inv_probability World.rand expand_probability 
    (fun () -> World.spawn (1) (self#get_pos) 
    (fun x -> ignore(new town x (gold_id))))
  
  method! smells_like_gold = 
    if (town_gold > 0) then Some gold_id else None
 
  method! forfeit_gold =
    with_inv_probability_or World.rand forfeit_gold_probability
    (fun () ->
      match town_gold with
      | 0  -> None
      | _  -> (town_gold <- (town_gold - 1)); Some (gold_id))
    (fun () -> None)


   method private do_action () = 
     self#produce_gold;
     self#expand
 
  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "town"
  method! draw_picture = self#draw_circle (Graphics.rgb 0x96 0x4B
                 0x00) Graphics.black (string_of_int town_gold)
  method! draw_z_axis = 1

  (* ### TODO: Part 4 Aging ### *)
  method! receive_gold ps =
    match ps with
    | [] -> []
    | _ -> 
      if not (List.mem ps gold_id)
      then (ignore (self#reset_life); ps)
      else ps
     
  (* ### TODO: Part 3 Actions ### *)
  
  (* ### TODO: Part 4 Aging ### *)

end
