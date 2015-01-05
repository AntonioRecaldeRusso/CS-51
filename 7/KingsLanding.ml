open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)              
let starting_gold = 500 
let cost_of_human = 10
let spawn_probability = 20
let gold_probability = 50
let max_gold_deposit = 3

(** King's Landing will spawn humans and serve as a deposit point for the gold
    that humans gather. It is possible to steal gold from King's Landing;
    however the city will signal that it is in danger and its loyal humans
    will become angry. *)

class kings_landing p : 
object
inherit world_object_i
  method forfeit_treasury : int -> world_object_i -> int
  method get_gold_event : int Event51.event
  method get_gold : int
end 
=
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)
  (* ### TODO: Part 3 Actions ### *)
  val mutable kings_gold = starting_gold

  (* ### TODO: Part 6 Custom Events ### *)
  val gold_event = Event51.new_event ()

  (***********************)
  (***** Initializer *****)
  (***********************)
  
  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event self#do_action 
   

  (**************************)
  (***** Event Handlers *****)
  (**************************)
  
  (* ### TODO: Part 3 Actions ### *)
  method private do_action = fun () ->
    with_inv_probability World.rand spawn_probability (fun () -> 
      if kings_gold >= cost_of_human
      then (kings_gold <- kings_gold - cost_of_human; self#generate_human (); ())
      else ())

  method private produce_gold = 
    with_inv_probability World.rand gold_probability 
      (fun () -> kings_gold <- (kings_gold + 1)) 

   
  method! receive_gold gold_list =
    let gold_offered = List.length gold_list in
    let gold_accepted = 
      if (gold_offered > max_gold_deposit) 
      then max_gold_deposit 
      else gold_offered in
    kings_gold <- kings_gold + gold_accepted; 
    Event51.fire_event self#get_gold_event self#get_gold;
    []
    
   

  method forfeit_treasury n obj =
    let gold = kings_gold in
    self#danger obj;
    match (n >= kings_gold) with
    | true ->   (kings_gold <- 0); gold
    | false ->  (kings_gold <- (kings_gold - n)); n 

  (* ### TODO: Part 4 Aging ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  method private generate_human () = 
    ignore(with_inv_probability_or World.rand 2
      (fun () -> new Lannister.lannister self#get_pos (self :> world_object_i))
      (fun () -> new Baratheon.baratheon self#get_pos (self :> world_object_i)))
 
									     
  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "kings_landing"
  method! draw = self#draw_circle (Graphics.rgb 0xFF 0xD7 0x00) Graphics.black (string_of_int kings_gold)
  method! draw_z_axis = 1


  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)
  

  (**********************************)
  (***** King's Landing Methods *****)
  (**********************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)
  method get_gold_event : int Event51.event = gold_event
  method get_gold : int = kings_gold

end
