open Core.Std
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let gold_theft_amount = 1000

(* ### Part 4 Aging ### *)
let dragon_starting_life = 20

(* ### Part 2 Movement ### *)
let dragon_inverse_speed = Some 10

class dragon (p : int * int) (kings_landing : KingsLanding.kings_landing) (dany) :  Movable.movable_t =
object (self)
  inherit Movable.movable p dragon_inverse_speed 

  (******************************)
  (***** Instance Variables *****)
  (******************************)
  
  (* ### TODO: Part 3 Actions ### *)
  val mutable gold_stolen = 0


  (* ### TODO: Part 6 Events ### *)
  val mutable life = dragon_starting_life


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event self#do_action 
    

  method private do_action () = 
    self#steal_gold;
    self#throw_away_gold;

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private steal_gold =
   
    if gold_stolen > 0 then () else
    match (self#get_pos = kings_landing#get_pos) with
    | false -> ()
    | _ -> 
      let gold = (kings_landing#forfeit_treasury 
        gold_theft_amount (self :> world_object_i)) in
        gold_stolen <- gold_stolen + gold
     

  (* ### TODO: Part 6 Custom Events ### *)
  method private throw_away_gold =
    if dany#get_pos = self#get_pos
    then (gold_stolen <- 0; self#decide_next_action)

  method private decide_next_action =
    if kings_landing#get_gold < (gold_theft_amount / 2) 
    then self#die
    else ()  


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "dragon"

  method! draw = self#draw_circle Graphics.red Graphics.black (string_of_int gold_stolen)

  method! draw_z_axis = 3


  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)
  method! receive_damage =
    life <- life - 1;
    if life <= 0 then self#die else ()

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction = 
    if gold_stolen = 0
    then self#next_direction_default
    else World.direction_from_to self#get_pos dany#get_pos

  (* ### TODO: Part 6 Custom Events ### *)
  method private next_direction_default = 
    World.direction_from_to (self#get_pos) kings_landing#get_pos
end
