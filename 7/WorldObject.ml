open Core.Std
open WorldObjectI
open Event51

(** An abstract implementation of world_object_i that provides some helper
    functionality.

    All world_object objects add themselves to the world at point initial_p upon
    creation. *)
class world_object (initial_p:int*int) : world_object_i =
object (self)

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val die_event : unit Event51.event = Event51.new_event ()
  val danger_event : world_object_i Event51.event = Event51.new_event ()

  val mutable pos : int*int = initial_p

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    World.add pos (self :> world_object_i) ;

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  method private set_pos (p:int*int) : unit =
    if World.can_move p then begin
      World.remove_must_exist pos (self :> world_object_i) ;
      World.add p (self :> world_object_i) ;
      pos <- p
    end else
      ()

  (*******************************)
  (***** WorldObject Methods *****)
  (*******************************)

  method private register_handler
    : 'a. 'a Event51.event -> ('a -> unit) -> unit =
    fun e f ->
      let id = Event51.add_listener e f in
      ignore(Event51.add_listener die_event
        (fun () -> Event51.remove_listener e id))

  method private move d =
    self#set_pos (Direction.move_point pos d)

  method private draw_circle bg fg text =
    Draw.circle pos World.obj_width World.obj_height bg fg text

  method private draw_status_bar c v =
    let h = World.obj_height / 10 in
    Draw.status_bar pos World.obj_width World.obj_height c h v

  method get_name = "object"

  method get_pos = pos

  method draw = self#draw_circle Graphics.green Graphics.black ""

  method draw_z_axis = 1

  method is_obstacle = false

  method smells_like_gold = None

  method forfeit_gold = None

  method receive_gold ps = ps

  method receive_damage = ()

  method get_die_event = die_event

  method die =
    Event51.fire_event die_event () ;
    World.remove_must_exist pos (self :> world_object_i)

  method get_danger_event = danger_event

  method danger o = Event51.fire_event danger_event o


end

