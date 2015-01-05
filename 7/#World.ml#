open Core.Std
open WorldObjectI
open Event51


(*************************)
(***** Configuration *****)
(*************************)

(** The icon width for each position. *)
let obj_width : int = 12

(** The icon height for each position. *)
let obj_height : int = 12

(** A random number generator for the world.  All random events happening in
   the world should use this random number generator. *)
let rand : int -> int = Random.self_init () ; Random.int

(** The world has size x size positions *)
let size : int = 50

(***********************)
(***** World State *****)
(***********************)

(** The state of the world -- a matrix of positions, where each
   position contains a list of world objects. *)
let world : world_object_i list array array =
  Array.make_matrix ~dimx:size ~dimy:size []

(****************************)
(***** World Operations *****)
(****************************)

(** Clear out all objects from the world. *)
let reset () : unit =
  Array.iter ~f:(fun row -> Helpers.array_map_modify (fun _ -> []) row)
             world

(** Get all objects associated with a location in the world. *)
let get ((x,y):int*int) : world_object_i list =
  world.(x).(y)

(** Set a location in the world to contain a new list of objects. *)
let set ((x,y):int*int) (wos:world_object_i list) : unit =
  world.(x).(y) <- wos

(** Modify a location in the world with value os to contain (f os). *)
let modify (p:int*int) (f:world_object_i list -> world_object_i list) : unit =
  set p (f (get p))

(** Add an object to the list of world objects at a location. *)
let add (p:int*int) (w:world_object_i) : unit =
  modify p (fun wos -> if List.mem wos w then wos else w::wos)

(** Remove an object from the list of world objects at a location. Does
    nothing if the object was not in the list. *)
let remove (p:int*int) (w:world_object_i) : unit =
  modify p (fun wos -> List.filter ~f:(fun w' -> w' <> w) wos)

(** Same as remove but fails if the object is not in the list. *)
let remove_must_exist (p:int*int) (w:world_object_i) : unit =
  assert (List.mem (get p) w) ;
  remove p w

(** Fold over all objects in the world. *)
let fold (f:world_object_i -> 'a -> 'a) (i:'a) : 'a =
  Array.fold_right
    ~f:(fun row accum ->
       Array.fold_right
         ~f:(fun os accum' -> List.fold_right ~f ~init:accum' os)
         ~init:accum
         row)
    ~init:i
    world

(** Call a function for all indices in the world. *)
let indices (f:int*int -> unit) : unit =
  Array.iteri ~f:(fun x -> Array.iteri ~f:(fun y _ -> f (x,y))) world

(** True if the world contains the point (x,y). *)
let check_bounds ((x,y):int*int) : bool =
  x >= 0 && x < size && y >= 0 && y < size

(** Iterate of all world objects along with their corresponding location. *)
let iteri (f:int*int -> world_object_i -> unit) : unit =
  indices (fun p -> List.iter ~f:(f p) (get p))

(** True if the world contains no objects at point p. *)
let is_empty p = get p = []

(** All objects within n spaces from location (x,y). *)
let objects_within_range ((x,y):int*int) (n:int) : world_object_i list =
  let xlow = max (x-n) 0 in
  let ylow = max (y-n) 0 in
  let xhigh = min (x+n) (size-1) in
  let yhigh = min (y+n) (size-1) in
  let coords = Helpers.cross (Helpers.range xlow xhigh)
    (Helpers.range ylow yhigh) in
  List.fold_right ~f:(fun p t -> get p @ t) ~init:[] coords

(** The next available point to (x,y) is a random close by element which is
    both on the grid and is unoccupied. *)
let rec next_available ((x,y):int*int) : int*int =
  if not (check_bounds (x,y)) then
    next_available (Helpers.bound 0 (size-1) x,Helpers.bound 0 (size-1) y)
  else if not (is_empty (x,y)) then
    next_available (Direction.move_point (x,y) (Some (Direction.random rand)))
  else (x,y)

(****************************)
(***** Spawning Objects *****)
(****************************)

(** [spawn n p f] will call f n times on points near p which are both on the
    grid and unoccupied. *)
let rec spawn (n:int) (p:int*int) (f:int*int -> unit) : unit =
  if n <= 0 then () else
    let p' = next_available p in
    f p' ;
    spawn (n-1) p' f

  (** [spawn_iter num num_spawn barrier f] will call [spawn n p f] num times
      where p is a randomly chosen point.  [barrier] will be called between
      spawns. *)
let rec spawn_iter (num_iter:int)
                   (num_spawn:int)
                   (barrier:unit -> unit)
                   (f:int*int -> unit) : unit =
  if num_iter = 0 then () else begin
    barrier () ;
    spawn (rand num_spawn) (rand size, rand size) f ;
    spawn_iter (num_iter-1) num_spawn barrier f
  end

(**************************)
(***** World Movement *****)
(**************************)

(** True if there are any obstacles at point p. *)
let has_obstacles (p:int*int) : bool =
  List.fold_left ~f:(||) ~init:false
    (List.map ~f:(fun o -> o#is_obstacle) (get p))

(** True if there are any obstacles in the path. *)
let path_has_obstacles (ps:(int*int) list) : bool =
  List.fold_left ~f:(||) ~init:false (List.map ~f:has_obstacles ps)

(** An object can move to point p if it is in bounds and doesn't contain any
    obstacles. *)
let can_move (p:int*int) : bool =
  check_bounds p && not (has_obstacles p)

(** If the natural path from p1 to p2 contains no obstacles then the direction
    from p1 to p2 is the natural direction.  Otherwise it is random. *)
let direction_from_to (p1:int*int) (p2:int*int) : Direction.direction option =
  assert (check_bounds p1 && check_bounds p2) ;
  if path_has_obstacles (Direction.natural_path p1 p2)
  then Some (Direction.random rand)
  else Direction.natural p1 p2

(******************)
(***** EVENTS *****)
(******************)

(** Fires when objects should perform their action. *)
let action_event : unit Event51.event = Event51.new_event ()

(** Fires when objects should move. *)
let move_event : unit Event51.event = Event51.new_event ()

(** Fires when objects should age. *)
let age_event : unit Event51.event = Event51.new_event ()
