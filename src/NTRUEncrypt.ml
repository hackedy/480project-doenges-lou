(** The standard NTRU cryptosystem *)
module Poly = Polynomial

(** "A trusted party chooses public parameters (N, p, q, d) with N and p prime,
    gcd(p, q) = gcd(N, q) = 1, and q > (6d + 1)p." (p.419) *)
type ntru_params =
  { phi : Poly.t (** [x^N - 1], probably *)
  ; p : Int.t    (** some prime *)
  ; d : int      (** any integer *)
  ; q : Int.t    (** an integer such that q > (6d + 1)p and gcd(p, q) = gcd(N, q) = 1 *)
  }

let little_ntru_params =
  { (* phi = x^37 - 1 *)
    phi = Poly.sub (Poly.make_monomial Int.one 37) Poly.one
  ; p = Int.of_int 13
  ; q = Int.of_int 899888521
  ; d = 11
  }

(** The NTRUOperations module implements common operations in NTRU and NTRU-like cryptosystems. *)
module NTRUOperations = struct
  (** Compute the degree of the modulus in the given NTRU parameters. *)
  let rank params =
    Poly.deg params.phi

  let mul_star params f g =
    Poly.mul_star params.phi f g

  let mul_star_modp params f g =
    Poly.mul_star_mod params.phi params.p f g

  let mul_star_modq params f g =
    Poly.mul_star_mod params.phi params.q f g

  let gen_random_Td1d params =
    Poly.random_trit_poly_with_counts (rank params) (params.d + 1) params.d

  let gen_random_Tdd params =
    Poly.random_trit_poly_with_counts (rank params) params.d params.d

  let invert_Rp params f =
    Poly.invert_mod params.phi params.p f

  let invert_Rq params f =
    Poly.invert_mod params.phi params.q f

  let center_lift_q params poly =
    Poly.center_lift params.q poly

  let center_lift_p params poly =
    Poly.center_lift params.p poly
end

type keypair =
  { pubkey : Poly.t;
    privkey_f : Poly.t;
    privkey_f_invp : Poly.t;
    privkey_g : Poly.t
  }

(** [BookNTRU] includes key generation, encryption, and decryption algorithms
    for the standard NTRU cryptosystem defined in the textbook. *)
module BookNTRU = struct
  include NTRUOperations

  (** Given parameters and some choice of [f] and [g], invert [f] in [Rq] and
      [Rp] and compute the associated public/private key pair (f, h). *)
  let compute_keypair params f g =
    let f_invp = invert_Rp params f in
    let f_invq = invert_Rq params f in
    let h = mul_star params f_invq g in
    { privkey_f = f;
      privkey_f_invp = f_invp;
      privkey_g = g;
      pubkey = h
    }

  (** Generate a random set of NTRU keys. *)
  let rec keygen params =
    let f = gen_random_Td1d params in
    let g = gen_random_Tdd params in
    try compute_keypair params f g
    with _ -> keygen params

  (** Encrypt the plaintext with the given public key. *)
  let encrypt params pubkey plaintext =
    let r = gen_random_Tdd params in
    let pr = Poly.scale params.p r in
    let pr_h = mul_star params pr pubkey in
    center_lift_q params (Poly.add pr_h plaintext)

  (** Decrypt the given ciphertext using the private key information in [key]. *)
  let decrypt params key ciphertext =
    let fc = mul_star params key.privkey_f ciphertext in
    let a = center_lift_q params fc in
    let f_invp_a = mul_star params key.privkey_f_invp a in
    center_lift_p params f_invp_a
end
