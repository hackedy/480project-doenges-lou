(** The [Ring.RING] signature defines common functions for ring modules. *)
module type RING = sig
  (** A ring has a type [t] of elements. *)
  type t

  (** A ring has an additive identity. *)
  val zero : t

  (** A ring has a multiplicative identity. *)
  val one : t

  (** A ring has an addition operation. *)
  val add : t -> t -> t

  (** A ring has an additive inverse operation. *)
  val neg : t -> t

  (** A ring has a multiplication operation. *)
  val mul : t -> t -> t

  (** A ring has a subtraction map. *)
  val sub : t -> t -> t

  (** A ring has finite powers of its elements. *)
  val pow : t -> int -> t

  (** We can embed Z into a ring. *)
  val of_int : int -> t

  (** Return a string representation for the ring element. *)
  val string_of : t -> string

  (** Test two ring elements for equality. *)
  val eq : t -> t -> bool
end
