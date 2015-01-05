open Core.Std
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let town_limit = 200

(** The Wall will spawn a white walker when there are enough towns
    in the world. *)
class wall p (city : KingsLanding.kings_landing): world_object_i =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO Part 6 Custom Events ### *)
  initializer
    self#register_handler World.action_event self#do_action

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO Part 6 Custom Events ### *)
  method private do_action () = 
    if ((self#count_smelly_towns > town_limit) && not(self#white_walker_in_map))
    then (print_string "white_walkers! "; flush_all (); self#spawn_white_walker)
    else ()

  (********************************)
  (******** Helper Methods ********)
  (********************************)
  method private count_smelly_towns =
    World.fold (fun x y -> 
      if (x#smells_like_gold = None) then y
      else y + 1) 0

  method private spawn_white_walker =
    ignore (new WhiteWalker.white_walker self#get_pos city self)

  method private white_walker_in_map =
    World.fold (fun x y -> (x#get_name = "white_walker") || y) false


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method! get_name = "wall"

  method! draw = self#draw_circle (Graphics.rgb 70 100 130) Graphics.white "W"

  method! draw_z_axis = 1


end

