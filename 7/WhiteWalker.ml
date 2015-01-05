open Core.Std
open Helpers
open WorldObject
open WorldObjectI
open Event51




(* ### Part 2 Movement ### *)
let walker_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_destroyed_objects = 100

(** A White Walker will roam the world until it has destroyed a satisfactory
    number of towns *)
class white_walker (p : int * int) (kings_landing : KingsLanding.kings_landing) 
                   (wall) : Movable.movable_t =
object (self)
  inherit Movable.movable p walker_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable vanquished_towns = 0

  val mutable dangerous = true

 
  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event self#do_action 

   method private do_action () =
     self#vanquish;
     self#decide_next_action;
      

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
   method private vanquish = 
     if dangerous then 
       let neighbors = (World.get self#get_pos) in  
       let rec vanquish_town town_list = 
         match town_list with
         | [] -> ()
         | hd :: tl -> 
           (match (hd#smells_like_gold <> None) with
            | true -> (vanquished_towns <- vanquished_towns + 1; hd#die; 
                       if vanquished_towns >= max_destroyed_objects
                       then (dangerous <- false))
            | _ -> vanquish_town tl) in
   vanquish_town neighbors

  (* ### TODO: Part 6 Custom Events ### *)
   method private decide_next_action =
     if (not dangerous && (self#get_pos = wall#get_pos))
     then self#die
     else () 

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "white_walker"
  method! draw = self#draw_circle (Graphics.rgb 0x89 0xCF 0xF0) Graphics.black 
                                  (string_of_int vanquished_towns)
  method! draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  method! next_direction =
    if dangerous 
    then self#next_direction_default
    else World.direction_from_to self#get_pos wall#get_pos

  (* ### TODO: Part 6 Custom Events ### *)
  method private next_direction_default = 
    with_inv_probability_or (World.rand) (World.size / 2 ) 
      (fun () -> World.direction_from_to (self#get_pos) (kings_landing#get_pos))
      (fun () -> Some (Direction.random World.rand))
end
