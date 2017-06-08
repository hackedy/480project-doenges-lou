(* This is a cryptosystem based upon the implementation given by Doroz that's
   essentially a single-key variant of the multi-key scheme *)

module Poly = Polynomial
module Chi = RandomPolynomials
module PolyVec = PolynomialVector

type public_parameters =
  { (* ladder of d primes q_1 > q_2 > ... > q_d *)
    prime_moduli : Int.t array
    (* degree of the polynomial phi(x) = x^n + 1 used in defining the ring R = Z[x] / phi(x) *)
  ; degree : int
    (* Standard deviation r for the noise distribution chi *)
  ; std_dev : int
    (* The bitwidth of each modulus q_i *)
  ; bitwidths : int array
    (* The polynomial phi(x) = x^degree + 1 *)
  ; phi : Poly.t
  }

let make_public_params prime_moduli degree std_dev =
  let validate_moduli i cur =
    if i < Array.length prime_moduli - 1
    then assert (Int.gt cur prime_moduli.(i+1))
  in
  Array.iteri validate_moduli prime_moduli;
  assert (degree > 3);
  { prime_moduli = prime_moduli
  ; degree = degree
  ; std_dev = std_dev
  ; bitwidths = Array.map Int.bitwidth prime_moduli
  ; phi = Poly.add Poly.one (Poly.make_monomial Int.one degree)
  }

(* A plaintext is a bit. *)
type plaintext = bool

(* A ciphertext is a polynomial tagged with its encryption level. *)
type ciphertext =
  { level : int
  ; text : Poly.t
  }

(* A secret key is an array of polynomials f_i for each level of the ladder. *)
type secret_key = Poly.t array

(* An evaluation key is a vector of polynomials zeta for each level of the ladder *)
type evaluation_key = PolyVec.t array

(* A public key has the form h_0 = 2 g_0 f_0^-1 *)
type public_key = Poly.t


let sample params =
  let bound = (float_of_int params.std_dev) *. sqrt (float_of_int params.degree) in
  let sample_coe _ = Chi.sample_coefficient (int_of_float bound) in
  let coefficients = Array.init (params.degree + 1) sample_coe in
  Poly.make coefficients

let invert_mod params level : Poly.t -> Poly.t =
  Poly.invert_mod params.phi params.prime_moduli.(level)

(* Intermediate state for the key generation algorithm *)
type keygen_level =
  { f : Poly.t
  ; f_inv : Poly.t
  ; u : Poly.t
  ; g : Poly.t
  ; h : Poly.t
  }

let two = Int.of_int 2

let keygen params : secret_key * public_key * evaluation_key =
  let keygen_level level q =
    Printf.printf "doing keygen for level %d\n" level; flush stdout;
    let mul = Poly.mul_star_mod params.phi q in
    Printf.printf "Sampling u and g\n"; flush stdout;
    (* u_i, g_i <- Chi *)
    let u = Poly.center_lift q (sample params) in
    let g = Poly.center_lift q (sample params) in
    (* f_i = 2u_i + 1 *)
    Printf.printf "computing f\n"; flush stdout;
    let f = Poly.add_mod q (Poly.scale two u) Poly.one in
    (* h_i = 2g_i * f_i^{-1}, computing inverse in R_i = Z/(q_i Z)[x]/phi *)
    Printf.printf "inverting f\n"; flush stdout;
    let f_inv = invert_mod params level f in
    Printf.printf "inverted f\n"; flush stdout;
    assert (Poly.one = mul f f_inv);
    let h = mul (Poly.scale two g) f_inv in
    Printf.printf "done generating for level %d\n" level; flush stdout;
    { f = f
    ; f_inv = f_inv
    ; u = u
    ; g = g
    ; h = h
    }
  in
  let generate_eval_key stack level =
    Printf.printf "making eval key for level %d" level;
    flush stdout;
    let q_prev = params.prime_moduli.(level - 1) in
    let mul = Poly.mul_star_mod params.phi q_prev in
    let prev_f = stack.(level - 1).f in
    let bits = params.bitwidths.(level) in
    (* zeta_i,tau = h_i s_i,tau + 2e_i,tau + 2^tau f_inv^2 in the ring R_(i-1) *)
    let mk_zeta tau =
      Printf.printf "tau = %d" tau; flush stdout;
      let s = sample params in
      let e = sample params in
      let hs = mul stack.(level).h s in
      let e' = Poly.scale two e in
      let f2 = mul prev_f prev_f in
      let powtau = Int.pow two tau in
      let zeta = Poly.add (Poly.add hs e') (Poly.scale powtau f2) in
      Poly.center_lift q_prev zeta
    in
    Array.init bits mk_zeta
  in
  let levels = Array.length params.prime_moduli in
  Printf.printf "making key stack\n";
  flush stdout;
  let key_stack = Array.mapi keygen_level params.prime_moduli in
  let mk_eval_key i =
    if i = 0
    then Array.make params.bitwidths.(0) Poly.one
    else generate_eval_key key_stack i
  in
  let eval_key = Array.init levels mk_eval_key in
  let pub_key = key_stack.(0).h in
  let priv_key = Array.map (fun keys -> keys.f) key_stack in
  (priv_key, pub_key, eval_key)

let encrypt params pubkey plaintext =
  let b_poly = if plaintext then Poly.one else Poly.zero in
  let q = params.prime_moduli.(0) in
  let mul = Poly.mul_star params.phi in

  let s = sample params in
  let e = sample params in
  let text = Poly.add (mul pubkey s) (Poly.add (Poly.scale two e) b_poly) in

  { level = 0
  ; text = Poly.center_lift q text
  }

let decrypt params secret ciphertext =
  let level = ciphertext.level in
  let key = secret.(level) in
  let q = params.prime_moduli.(level) in
  let fc = Poly.mul_star params.phi key ciphertext.text in
  let p = Poly.mod_coeff two (Poly.center_lift q fc) in
  if p = Poly.one
  then true
  else if p = Poly.zero
  then false
  else failwith (Printf.sprintf "couldn't decrypt a polynomial of degree %d" (Poly.deg p))

(* Add two ciphertexts. Addition does not bump the level, but c1 and c2 do need
   to have the same level. *)
let add params c1 c2 =
  let level = c1.level in
  assert (c1.level = c2.level);
  let modulus = params.prime_moduli.(level) in
  let c' = Poly.add_mod modulus c1.text c2.text in
  { level = c1.level
  ; text = c'
  }

let relinearize params eval_key c =
  let level = c.level in
  let next_bitwidth = params.bitwidths.(level+1) in
  let c_bits = PolyVec.bitslice next_bitwidth c.text in
  let q_next = params.prime_moduli.(level + 1) in
  let dot = PolyVec.dot_star params.phi in
  let c' = dot eval_key.(level + 1) c_bits in
  { level = level
  ; text = Poly.center_lift q_next c'
  }

let modulus_switch params (c : ciphertext) : ciphertext =
  let level = c.level in
  let level' = level+1 in
  let q_cur = params.prime_moduli.(level) in
  let q_next = params.prime_moduli.(level') in
  let text' = Poly.rounded_division_mod2 q_next q_cur c.text in
  { text = text'
  ; level = level'
  }

let bump_level params eval_key c =
  modulus_switch params (relinearize params eval_key c)

let mul params eval_key c1 c2 =
  let level = c1.level in
  assert (c1.level = c2.level);
  let modulus = params.prime_moduli.(level) in
  let mul = Poly.mul_star_mod params.phi modulus in
  let c' = { level = level
           ; text = mul c1.text c2.text
           }
  in
  bump_level params eval_key c'
