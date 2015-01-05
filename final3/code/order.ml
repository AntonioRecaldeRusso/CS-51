type order = EQUAL | LESS | GREATER

module type COMPARABLE =
sig 
  type t

  val compare : t -> t -> order
  val to_string : t -> string
end

module IntCompare : COMPARABLE with type t = int =
struct
  type t = int

  let compare x y =
    if x < y then LESS else if x > y then GREATER else EQUAL
  let to_string = string_of_int

end
