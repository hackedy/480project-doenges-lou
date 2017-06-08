(** The LHE Cryptosystem based on NTRU *)
module Poly = Polynomial
module NTRU = NTRUEncrypt
open NTRUEncrypt

(** Claim: choosing [q > 2(alpha)(2pN)^(2mu)], under the NTRU encryption scheme
    with parameters (N,P,q,d) we can achieve a leveled homomorphic cryptosystem
    of additive level [alpha] and multiplicative level [mu].

    Running [generate_params_for_level alpha mu N p] will compute a prime [q]
    satisfying the required inequality and return a params struct. *)
let generate_params_for_level additions multiplications dimension p =
  let n = Int.of_int dimension in
  let alpha = Int.of_int additions in
  let mu = multiplications in

  let twopn = Int.mul Int.two (Int.mul p n) in
  let twopnmu = Int.pow twopn (2 * mu) in
  let lower_bound = Int.mul (Int.mul Int.two alpha) (twopnmu) in
  let q = Int.nextprime lower_bound in

  let d = dimension / 5 in
  assert (d > 0);

  { phi = Poly.sub (Poly.make_monomial Int.one dimension) Poly.one
  ; p = p
  ; q = q
  ; d = d
  }

(** Convert a params struct to a string. *)
let show_params params =
  Printf.sprintf "N = %d\tp = %s\nq = %s\n"
                 (Poly.deg params.phi)
                 (Int.string_of params.p)
                 (Int.string_of params.q)

let little_ntru_lhe_params =
  { phi = Poly.sub (Poly.make_monomial Int.one 37) Poly.one (* phi = x^37 - 1 *)
  ; p = Int.of_int 13
  ; q = Int.of_int 8743753
  ; d = 11
  }

(** This module implements the book's presentation of LHE. *)
module BookNTRULHE = struct
  include NTRUOperations

  (** Given f and g, invert f in Rp and Rq and compute keys using the inverses and g. *)
  let compute_keypair params f g =
    let f_invp = invert_Rp params f in
    let f_invq = invert_Rq params f in
    let h = mul_star params f_invq g in
    { privkey_f = f;
      privkey_f_invp = f_invp;
      privkey_g = g;
      pubkey = h
    }

  (** Generate keys for LHE. *)
  let rec keygen params =
    let f0 = gen_random_Td1d params in
    let g0 = gen_random_Tdd params in
    let f  = Poly.add Poly.one (Poly.scale params.p f0) in
    let g  = Poly.scale params.p g0 in
    try compute_keypair params f g
    with _ -> keygen params

  (** [encrypt params pubkey plaintext] will encrypt [plaintext] under the given [pubkey]. *)
  let encrypt params pubkey plaintext =
    let r = gen_random_Tdd params in
    let pr = Poly.scale params.p r in
    let pr_h = mul_star params pr pubkey in
    center_lift_q params (Poly.add pr_h plaintext)

  (** Compute the nth power of [f] in R. *)
  let pow_star params level f =
    let rec go level acc =
      if level = 1
      then acc
      else go (level - 1) (mul_star params f acc)
    in
    go level f

  (** Running [decrypt params key ciphertext] will decrypt the given ciphertext
      with the given secret key. The optional [level] parameter controls how
      many multiplicative levels deep the ciphertext should be considered. *)
  let decrypt params key ?(level=1) ciphertext =
    let f' = pow_star params level key.privkey_f in
    let f'c = mul_star params f' ciphertext in
    let a = center_lift_q params f'c in
    center_lift_p params a
end
