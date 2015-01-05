open Core.Std
open Helpers

(** Class type for objects which age over time.  After aging an ageable object
    will eventually die. *)
class type ageable_t =
object
  inherit Movable.movable_t

  (** Draw just the picture portion of this object. After drawing this
      picture, the life bar for the object will be drawn on top. *)
  method draw_picture : unit

  (** Reset the life of this object to be the maximum amount. *)
  method reset_life : unit

end

class ageable p inv_speed starting_lifetime max_lifetime : ageable_t =
object (self)
  inherit Movable.movable p inv_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable lifetime = starting_lifetime

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 4 Aging ### *)
  initializer
    self#register_handler World.age_event self#do_age

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  method private do_age () =
    lifetime <- lifetime - 1;
    if lifetime <= 0 then self#die
    else () 


  (**************************)
  (***** Helper Methods *****)
  (**************************)

  method private draw_life : unit =
    let v = Float.of_int lifetime /. Float.of_int max_lifetime in
    self#draw_status_bar Graphics.red v

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method! draw : unit =
    self#draw_picture ;
    self#draw_life

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)
  method draw_picture : unit = self#draw_circle Graphics.white Graphics.black ""
  method reset_life = lifetime <- max_lifetime
end
