open Core.Std
(** An Interface for events. *)
module type EVENT =
sig
  (** The event listener identifier type. *)
  type id

  (** The event type. *)
  type 'a event

  (** Create a new event. *)
  val new_event : unit -> 'a event

  (** Add a listener to an event which is called every time the event is fired.
      Return an identifier for the listener. *)
  val add_listener : 'a event -> ('a -> unit) -> id

  (** Remove a listener from being called when an event is fired. Has no effect
      if the listener is not waiting for the event. *)
  val remove_listener : 'a event -> id -> unit

  (** Add a listener to an event which will be called once when the event is
      fired. *)
  val add_one_shot_listener : 'a event -> ('a -> unit) -> unit

  (** Signal that an event has occurred.  The 'a value is passed to each
      function waiting for the event. *)
  val fire_event : 'a event -> 'a -> unit

  (** For every event value x fired from event e, map f e will fire an event
      with value (f x). *)
  val map : ('a -> 'b) -> 'a event -> 'b event

  (** For every event value x fired from event e that satisfies p, filter p e
      will fire an event with value x. *)
  val filter : ('a -> bool) -> 'a event -> 'a event

  (** buffer n e will repeatedly fire an event containing a list of n values
      after event e has fired n times. *)
  val buffer : int -> 'a event -> 'a list event

  (** For every event value x_n fired from event e, integrate f i e will fire an
      event with value (fold f i [x_1; x_2; x_3; ...; x_n]) *)
  val integrate : ('a -> 'b -> 'b) -> 'b -> 'a event -> 'b event
end

module Event51 : EVENT =
struct
  type id = int
  type 'a waiter = {id : id ; action : 'a -> unit}
  type 'a event = 'a waiter list ref

  let id_counter = ref 0

  let new_id () : id =
    let i = !id_counter in
    incr id_counter ; i

  let new_event : unit -> 'a event = fun () -> ref []

  let add (w:'a waiter) (e:'a event) : id = e := w::(!e) ; w.id

  let add_listener (e:'a event) (f:'a -> unit) : id =
    let i = new_id () in add {id=i; action=f} e

  let remove_listener (e:'a event) (i:id) : unit =
    e := List.filter ~f:(fun w -> w.id <> i) (!e)

  let add_one_shot_listener e f =
    let id = ref (-1) in
    id := add_listener e (fun x -> remove_listener e !id ; f x)

  let fire_event (e:'a event) (v:'a) : unit =
    let waiters = !e in
    List.iter ~f:(fun w -> w.action v) waiters

  let map (f:'a -> 'b) (a_event:'a event) : 'b event =
    let b_event = new_event () in
    ignore(add_listener a_event (fun a -> fire_event b_event (f a))) ;
    b_event

  let filter (f:'a -> bool) (event:'a event) : 'a event =
    let f_event = new_event () in
    ignore(add_listener event begin fun a ->
      if f a then fire_event f_event a
    end) ;
    f_event

  let buffer (n:int) (event:'a event) : 'a list event =
    let b_event = new_event () in
    let buffer = ref [] in
    let counter = ref 0 in
    ignore(add_listener event begin fun a ->
      buffer := a::!buffer ;
      incr counter ;
      if !counter >= n then begin
        fire_event b_event (List.rev !buffer) ;
        buffer := [] ;
        counter := 0
      end
    end) ;
    b_event

  let integrate (f:'a -> 'b -> 'b) (i:'b) (event:'a event) : 'b event =
    let i_event = new_event () in
    let tally = ref i in
    ignore(add_listener event begin fun a ->
      tally := f a !tally ;
      fire_event i_event !tally
    end) ;
    i_event
end

