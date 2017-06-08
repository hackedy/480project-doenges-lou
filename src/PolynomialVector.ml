module Poly = Polynomial

(* A PolynomialVector is an array of polynomials. *)
type t = Poly.t array

let dim vec =
  Array.length vec

(* given ring operations for multiplication and addition of polynomials, compute
   the dot product of v1 and v2 *)
let dot_with mul add v1 v2 : Poly.t =
  let multiplied = BatArray.map2 mul v1 v2 in
  Array.fold_left add Poly.zero multiplied

let dot_star phi =
  dot_with (Poly.mul_star phi) Poly.add

(** Compute the function Bits(f) from LTV assuming coefficients can be stored in width bits *)
let bitslice width poly =
  let coeff_bits = Poly.coeff_bits width poly in
  let count = Array.length coeff_bits in
  (* now we transpose coeff_bits so we get an array of first bits from each
     coefficient, then an array of the second bits from each coefficient, etc *)
  let slice i =
    let ith_bit_of_coeff j = coeff_bits.(j).(i) in
    Poly.make (Array.init count ith_bit_of_coeff)
  in
  Array.init width slice
