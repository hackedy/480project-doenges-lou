type t = Polynomial.t array

(* How many polynomials are in this vector? *)
val dim : t -> int

(* Given a polynomial modulus phi(x), compute the innner product of two vectors
   using ops from the ring Z[x] / phi(x). *)
val dot_star : Polynomial.t -> t -> t -> Polynomial.t

(* Given a maximum bitwidth k for the coefficients of a polynomial p, produce an
   array of 1-bounded polynomials b_i such that sum_{i=1}^k 2^i b_i = p. *)
val bitslice : int -> Polynomial.t -> t
