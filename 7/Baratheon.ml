open Core.Std
open WorldObject
open WorldObjectI

(* Baratheons should travel in a random direction. *)
class baratheon p city : Human.human_t =
object
  inherit Human.human p city

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "baratheon"
  

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)
  method! next_direction_default : Direction.direction option = 
    Some (Direction.random World.rand)

end

