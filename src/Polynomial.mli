(** Polynomials in [Z[x]] and its quotients. *)
include Ring.RING

type coefficient = Int.t

(** Create a polynomial from an array of coefficients [|a_0; ...; a_n|]. If the
    list is empty, return the zero polynomial. The opaque type [t] is the type
    of polynomials. *)
val make : coefficient array -> t

(** Extract the coefficients of the given polynomial as an array. *)
val array_of : t -> coefficient array

(** Determine the degree of the given polynomial. *)
val deg : t -> int

(** Get the ith coefficient of the polynomial. *)
val coeff : t -> int -> coefficient

(** Given [f] and [g],
    return ([q], [r]) such that [f = qg + r] and [deg r < deg g]. *)
val div_rem : t -> t -> (t * t)

(** Build the monomial coefficient*x^power. *)
val make_monomial : coefficient -> int -> t

(** Map the given function over the cofficients of the polynomial and return the
    resulting polynomial. *)
val map_coeff : (coefficient -> coefficient) -> t ->  t

(** [mod_coeff p f] reduces the coefficients of f modulo p. *)
val mod_coeff : coefficient -> t -> t

(** Center-lift the coefficients of [p] modulo [modulus]. *)
val center_lift : coefficient -> t -> t

(** Multiply the polynomial by the given scalar. *)
val scale : coefficient -> t -> t

(** Running[random_trit_poly_with_counts length count_ones count_neg_ones]
    samples a polynomial from the uniform distribution T([count_ones],
    [count_neg_ones]) with a maximum polynomial degree of [length]. *)
val random_trit_poly_with_counts : int -> int -> int -> t

(** Find an inverse for f in the ring R = [Z/pZ[x]]. *)
val invert_mod : t -> coefficient -> t -> t

(** [mul_star phi f g] multiplies [f] and [g] in [Z[x]/<phi>]. *)
val mul_star : t -> t -> t -> t

(** [mul_star_mod phi p f g] multiplies [f] and [g] in [Z/pZ[x]/<phi>]. *)
val mul_star_mod : t -> Int.t -> t -> t -> t

(** Given [f] and [g],
    return ([q], [r]) such that [f = qg + r] and [deg r < deg g]. *)
val div_rem : t -> t -> t * t

(** Run the extended Euclidean algorithm on [a] and [b] and return [(s, t, g)]
    such that [g] is a gcd of [a] and [b] and [as + tb = g]. *)
val eea : t -> t -> t * t * t

(** Add the polynomials [a] and [b] in the ring [Z/pZ[x]]. *)
val add_mod : coefficient -> t -> t -> t

(* Run the extended Euclidean algorithm on [a] and [b] and return [(s, t, g)]
   such that [g] is a gcd of [a] and [b] in R and [as + tb = g] in R, where R is
   the ring [Z/pZ[x]]. *)
val eea_mod : coefficient -> t -> t -> t * t * t

(** Given [f] and [g], return ([q], [r]) such that [f = qg + r] in Z/pZ[x], with
    [deg r < deg g]. *)
val div_rem_mod : coefficient -> t -> t -> t * t

(** Evaluate the polynomial f at point x *)
val evaluate : t -> Int.t -> Int.t

(* Compute an array of bit-arrays for each coefficient. *)
val coeff_bits : int -> t -> Int.t array array

(** Given integers [a] and [b], compute [a/b * f] and then round it to the
    closest polynomial g such that f = g modulo 2. *)
val rounded_division_mod2 : Int.t -> Int.t -> t -> t
