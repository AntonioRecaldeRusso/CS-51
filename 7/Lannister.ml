open Core.Std
open WorldObject
open WorldObjectI

(* Baratheons should travel in a random direction. *)
class lannister p city : Human.human_t =
object (self)
  inherit Human.human p city

  (******************************)
  (***** Instance Variables *****)
  (******************************)


  (* ### TODO: Part 5 Smart Humans *)
  val mutable last_direction : Direction.direction = 
    Direction.random World.rand

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "lannister"
  method! draw_picture = self#draw_circle Graphics.yellow Graphics.black 
                           (string_of_int (List.length self#get_human_gold))

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)
  method! next_direction_default : Direction.direction option = 
    if World.can_move (Direction.move_point self#get_pos (Some last_direction))
    then Some last_direction
    else (last_direction <- Direction.random World.rand; Some last_direction)

 
end
