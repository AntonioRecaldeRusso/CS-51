open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 2 Movement ### *)
let human_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_gold_types = 5

(* ### Part 4 Aging ### *)
let human_lifetime = 1000

(* ### Part 5 Smart Humans ### *)
let max_sensing_range = 5

(** Humans travel the world searching for towns to trade for gold.
    They are able to sense towns within close range, and they will return
    to King's Landing once they have traded with enough towns. *)
class type human_t =
object
  inherit Ageable.ageable_t

  method get_human_gold : int list
  method next_direction_default : Direction.direction option
end

class human p (home : world_object_i) : human_t =
object(self)
  inherit CarbonBased.carbon_based p human_inverse_speed 
          (World.rand human_lifetime) human_lifetime

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable human_gold = []

  (* ### TODO: Part 5 Smart Humans ### *)
  val sensing_range = World.rand max_sensing_range
  val gold_types = World.rand max_gold_types + 1

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable dragon_obj = None
  val mutable danger = false


  (***********************)
  (***** Initializer *****)
  (***********************)
  
  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event (fun () -> self#do_action);
    self#register_handler home#get_danger_event self#in_danger

 
  (* ### TODO: Part 6 Custom Events ### *)    

  (**************************)
  (***** Event Handlers *****)
  (**************************)
  method private do_action =
    self#deposit_gold;
    self#extract_gold;
    if danger then self#attack else ()  

  (* ### TODO: Part 6 Custom Events ### *)
  method private in_danger dragon =
    dragon_obj <- Some dragon;
    danger <- true;
    self#register_handler dragon#get_die_event 
      (fun _ -> danger <- false; dragon_obj <- None)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
 
  method private deposit_gold =   
   let rec deposit lst =
     match lst with
     | [] -> ()
     | hd :: tl -> human_gold <- hd#receive_gold human_gold; 
                   deposit tl in
   deposit ((World.get self#get_pos) :> 'a list)
  
  method private extract_gold = 
    let rec extract lst =
      
      match lst with
      | [] -> ()
      | hd :: tl -> if (hd#smells_like_gold <> None) then
        (match hd#forfeit_gold with
         | None -> extract tl
         | Some x -> human_gold <- (x :: human_gold); 
                     extract tl) 
                    else extract tl in
    extract (World.get self#get_pos) 

  method! private forfeit_gold =
    let tmp = List.length human_gold in
    human_gold <- []; Some tmp
        
  
  (* ### TODO: Part 5 Smart Humans ### *)
  method get_human_gold : int list = human_gold

  method private magnet_gold : world_object_i option =
    let obj_list = World.objects_within_range self#get_pos sensing_range in
    let smell town : bool = 
      match town#smells_like_gold with
      | None -> false
      | Some n -> not (List.mem human_gold n) in
    let smell_list =
      List.filter obj_list ~f:(fun x -> smell x) in
    match smell_list with
    | [] -> None
    | hd :: tl -> Some 
      (List.fold tl ~f:(fun x y ->
         if (Direction.distance self#get_pos x#get_pos) >
            (Direction.distance self#get_pos y#get_pos)
         then y 
         else x) ~init:hd)
    
      

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "human"
  
  method! draw_picture = self#draw_circle (Graphics.rgb 0xC9
               0xC0 0xBB) Graphics.black (string_of_int (List.length human_gold))

  method! draw_z_axis = 2


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)


  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  method! next_direction : Direction.direction option =
    match dragon_obj with
    | Some obj -> World.direction_from_to self#get_pos obj#get_pos
    | None -> 
      if List.length (unique human_gold) >= max_gold_types
      then World.direction_from_to (self#get_pos) (home#get_pos)
      else match self#magnet_gold with
           | Some town -> World.direction_from_to self#get_pos town#get_pos 
	   | _ -> self#next_direction_default

  (* ### TODO: Part 5 Smart Humans ### *)
  method private next_direction_default : Direction.direction option = None


  (* ### TODO: Part 6 Custom Events ### *)
  method private attack =
    match dragon_obj with
    | Some x -> 
      if self#get_pos = x#get_pos
      then (x#receive_damage; self#die)
      else () 
    | None -> ()
 

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans ### *)

end
