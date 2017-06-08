(** Integers *)
include Ring.RING

(** Map the given machine integer into [Int.t]. *)
val int_of : t -> int

(** The integer 2. *)
val two : t

(** [inv n] returns [n] if [n = 1] or [n = -1] and throws [Invalid_argument] otherwise. *)
val inv : t -> t

(** [lt a b] checks if [a] is less than [b]. *)
val lt : t -> t -> bool

(** [gt a b] checks if [a] is greater than [b]. *)
val gt : t -> t -> bool

(** [geq a b] checks if [a] is greater than or equal to [b]. *)
val geq : t -> t -> bool

(** [gcd a b] computes the greatest common denominator of [a] and [b]. *)
val gcd : t -> t -> t

(** [modulo a b] reduces [a] modulo [b]. *)
val modulo : t -> t -> t

(** If [inv_mod p a] doesn't throw [Invalid_argument], it computes the inverse
    of [a] in the ring (hopefully field) [Z/pZ]. *)
val inv_mod : t -> t -> t

(** [div_rem a b = (q, r)] where [a = qb + r] and [r < b]. *)
val div_rem : t -> t -> t * t

(** [center_lift p n] is the center lift of [n] modulo [p]. *)
val center_lift : t -> t -> t

(** Given [k], [random k] returns a random integer [n] such that [0 <= n < k]. *)
val random : t -> t

(** Compute the absolute value of an integer. *)
val abs : t -> t

(** [bitwidth k] is the number of bits needed to store [abs k]. *)
val bitwidth : t -> int
(** [bits k] is the array of bits representing [k], or possibly an array of -1s
    and 0s if [k] is negative. *)
val bits : int -> t -> t array


(** Given integers [a] and [b], compute [a/b * k] and then round it to the
    closest integer n such that [n % 2 = k % 2]. *)
val rounded_division_mod2 : t -> t -> t -> t

(** Get the next prime bigger than the given integer. The return value is prime
    with high probability. *)
val nextprime : t -> t

(** Compute a prime number with at least the given bit width. *)
val primebits : int -> t
