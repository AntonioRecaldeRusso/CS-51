open Core.Std
open Event51
open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let spawn_dragon_gold = 500

(** Dany will spawn a dragon when King's Landing has collected a certain
    amount of gold. *)
class dany p city: world_object_i =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable dragon_standby = true

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 6 Custom Events ### *)
  initializer
    self#register_handler city#get_gold_event (fun _ -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  method private do_action =
    if (city#get_gold >= spawn_dragon_gold) && (not self#dragon_in_map)
    then (print_string "dragon! "; flush_all (); self#spawn_dragon)       

  (******************************)
  (******* Helper Methods *******)
  (******************************)
  method private spawn_dragon : unit = 
    ignore (new Dragon.dragon self#get_pos city self)

  method private dragon_in_map : bool =
    World.fold (fun x y -> (x#get_name = "dragon") || y) false
 

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "dany"

  method! draw= self#draw_circle Graphics.black (Graphics.rgb 0x80 0x00 0x80) "D"

  method! draw_z_axis = 1


  (* ### TODO: Part 6 Custom Events *)

end
