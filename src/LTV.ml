open Circuit
module Poly = Polynomial
module Chi  = RandomPolynomials


(** modified NTRU: the cryptosystem in Section 2 of LTV's paper *)

(** security parameter *)
module type SEC_PARAM = sig
  val kappa : float
end

module type LTVNTRU_PARAMS = sig
  (** deg(phi) = n *)
  (** n = omega(log kappa) *)
  val n   : float -> int
  val phi : float -> Poly.t

  val q   : float -> Int.t

  (** Discrete Gaussian standard deviation, temporarily hard coded *)
  (** r > omega(sqrtlog kappa)*)
  val r   : float -> float
end

module type LTVNTRU_LH_PARAMS = sig
  (** maximum circuit depth the scheme can homomorphically evaluate *)
  val d   : int

  val n : float -> int
  val phi : float -> Poly.t

  (** a ladder of decreasing moduli q0, ... , qd *)
  val qs  : float -> Int.t array

  val r   : float -> float
end

module LittleSecParam = struct
  let kappa = 72.71
end

module LittleLTVNTRUParams = struct
  let n   = fun k -> int_of_float (2.0 ** (ceil ((log(log k)) /. (log 2.0))))
  let q   = fun _ -> Int.of_int 20333
  let phi = fun k -> Poly.add_mod (q ()) (Poly.make_monomial Int.one (n k)) Poly.one
  let r   = fun k -> log k
end

module LittleLHLTVParams = struct
  let d   = 4
  let n   = fun k -> int_of_float (2.0 ** (ceil ((log(log k)) /. (log 2.0))))
  let qs   = fun _ -> Array.map Int.of_int [| 61909; 61879; 61357; 53407; 20333|]

  let phi = fun k -> Poly.add (Poly.make_monomial Int.one (n k)) Poly.one
  let r   = fun k -> log k
end


module LTVNTRUOperations = struct
  type params = { phi : Poly.t; n : int; q : Int.t; logq_ceil : int; r : float }
  type keypair = {pk : Poly.t; sk : Poly.t}

  let sample params _ =
    Poly.make (Array.init (params.n + 1)
         (fun _ -> Chi.sample_coefficient (int_of_float (params.r *. sqrt (float (params.n))))))

  (** sample a vector of polynomials of size n *)
  let sample_vec params n =
    Array.init n (fun _ -> sample params ())

  let mul_star params f g =
    Poly.mul_star params.phi f g

  let add params f g =
    Poly.add_mod params.q f g

  let invert params f =
    Poly.invert_mod params.phi params.q f

  let center_lift params poly =
    Poly.center_lift params.q poly

  let mod2 poly =
    abs (Int.int_of (Poly.evaluate poly Int.zero)) mod 2

  let vec_scale params f =
    Array.map (fun v -> mul_star params f v)

  let vec_add params v1 v2 =
    if (Array.length v1) != (Array.length v2)
    then
      failwith ("length does not match: len v1 = " ^ string_of_int (Array.length v1) ^ "  len v2 = " ^ string_of_int (Array.length v2))
      (* let _ = failwith ("length does not match" ^ (Array.fold_left (fun a b -> a ^ "     " ^ (Poly.string_of b)) "" v1)) *)
    else BatArray.map2 (fun f1 f2 -> add params f1 f2) v1 v2

  let pow params f =
    Array.init params.logq_ceil
      (fun i -> mul_star params (Poly.of_int (int_of_float (2. ** (float i)))) f)

  let to_binary n =
    let rec aux x acc =
      if x = 0
      then acc
      else aux (x/2) ((x mod 2)::acc)
    in Array.of_list (aux n [])

  (** convert a polynomial an array of binary representation of coefficients *)
  let poly_to_binary c =
    Array.map (fun i -> Array.map Int.of_int (to_binary (Int.int_of i))) (Poly.array_of c)

  (** Bit(c) *)
  let bit params c =
    let bin = poly_to_binary c in
    Array.init params.logq_ceil (fun i -> Poly.make (Array.init params.n (fun j -> bin.(j).(i))))

  let dot_mod params fs gs =
    Array.fold_left (add params)
      Poly.zero
      (BatArray.map2 (fun f g -> center_lift params (mul_star params f g)) fs gs)

  (** Basic operations of evaluating a circuit *)
  let cadd params c c' =
    center_lift params (add params c c')

  let cmult params c c' ek =
    let c0 = center_lift params (mul_star params c c') in
    dot_mod params (bit params c0) ek

end

module LTVNTRU (Sec : SEC_PARAM) (Params : LTVNTRU_PARAMS) = struct
  type keypair = {pk : Poly.t; sk : Poly.t}

  module Ops = LTVNTRUOperations
  open Ops

  let params =
    { phi = Params.phi Sec.kappa
    ; n = Params.n Sec.kappa
    ; q = Params.q Sec.kappa
    ; logq_ceil = int_of_float (ceil ((log (float (Int.int_of (Params.q Sec.kappa)))) /. (log 2.0)))
    ; r = Params.r Sec.kappa }

  let center_lift = center_lift params
  let sample = sample params
  let mul_star = mul_star params
  let add = add params
  let invert = invert params

  let compute_keypair f g =
    let f_inv = invert f in
    let h = center_lift (Poly.scale (Int.of_int 2) (mul_star g f_inv)) in
    { pk = h; sk = f }

  let rec keygen _ =
    let f' = sample () in
    let g  = sample () in
    (** so that f = 1 (mod 2) *)
    let f  = add (Poly.scale (Int.of_int 2) f') Poly.one in
    try compute_keypair f g
    with _ -> keygen ()

  (** to encrypt a bit: m \in {0,1}*)
  let encrypt pk m =
    let s = sample () in
    let e = sample () in
    center_lift
      (add (mul_star pk s)
         (add (Poly.scale (Int.of_int 2) e) (Poly.of_int m)))

  let decrypt sk c =
    let mu = center_lift (mul_star sk c) in
    mod2 mu

end


module SHLTV (Sec : SEC_PARAM) (Params : LTVNTRU_PARAMS) = struct
  type keypair = {pk : Poly.t; sk : Poly.t}

  module Ops = LTVNTRUOperations
  open Ops

  let params =
    { phi = Params.phi Sec.kappa
    ; n = Params.n Sec.kappa
    ; q = Params.q Sec.kappa
    ; logq_ceil = int_of_float (ceil ((log (float (Int.int_of (Params.q Sec.kappa)))) /. (log 2.0)))
    ; r = Params.r Sec.kappa }

  type pse_pair =
    { pk : Poly.t; sk : Poly.t; ek : Poly.t array }

  let center_lift = center_lift params
  let sample = sample params
  let sample_vec = sample_vec params
  let mul_star = mul_star params
  let add = add params
  let invert = invert params
  let vec_scale = vec_scale params
  let vec_add = vec_add params
  let bit = bit params
  let pow = pow params
  let dot_mod = dot_mod params
  let cadd = cadd params
  let cmult = cmult params

  let compute_keypair f g =
    let f_inv = invert f in
    let h = center_lift (Poly.scale (Int.of_int 2) (mul_star g f_inv)) in
    { pk = h; sk = f }

  let rec pksk_gen _ =
    let f' = sample () in
    let g  = sample () in
    let f  = add (Poly.scale (Int.of_int 2) f') Poly.one in
    try compute_keypair f g
    with _ -> pksk_gen ()

  let rec keygen _ : pse_pair =
    let pksk = pksk_gen () in
    let svec = sample_vec params.logq_ceil in
    let evec = sample_vec params.logq_ceil in
    let e = Array.map center_lift
        (vec_add
           (vec_add (vec_scale pksk.pk svec) (vec_scale (Poly.of_int 2) evec))
           (pow pksk.sk)) in
    { pk = pksk.pk; sk = pksk.sk; ek = e}


  let rec encrypt pk m =
    let s = sample () in
    let e = sample () in
    add (mul_star pk s) (add (Poly.scale (Int.of_int 2) e) (Poly.of_int m))

  let rec decrypt sk c =
    let mu = center_lift (mul_star sk c) in
    mod2 mu

  (** evaluation of l-variate boolean circuit c : 2^l -> 2 of depeth D *)
  let rec eval (c : Circuit.t) c_vec pk (ek : Poly.t array) =
    let _ = assert (Circuit.number_of_inputs c = Array.length c_vec) in
    let rec evaluate (c : Circuit.t) =
      match c with
        IN i -> c_vec.(i)
      | OUT g -> evaluate g
      | NOT g -> cadd (evaluate g) Poly.one
      | AND (g1, g2) -> cmult (evaluate g1) (evaluate g2) ek
      | OR  (g1, g2) ->
        let v1 = evaluate g1 in
        let v2 = evaluate g2 in
        cadd (cadd v1 v2) (cmult v1 v2 ek)
      | XOR (g1, g2) -> cadd (evaluate g1) (evaluate g2)
      | NOR (g1, g2) -> evaluate (NOT (OR (g1, g2)))
      | NAND (g1, g2) -> evaluate (NOT (AND (g1, g2)))
    in
    evaluate c

end


module LHLTV (Sec : SEC_PARAM) (Params : LTVNTRU_LH_PARAMS) = struct
  module Ops = LTVNTRUOperations
  open Ops

  type params = { phi : Poly.t; d : int; n : int; qs : Int.t array; logq_ceils : int array; r : float}

  type paramsi = { phi : Poly.t; n : int; q : Int.t; logq_ceil : int; r : float}

  let params =
    { phi = Params.phi Sec.kappa
    ; d = Params.d
    ; n = Params.n Sec.kappa
    ; qs = Params.qs Sec.kappa
    ; logq_ceils = Array.map
          (fun q -> int_of_float (ceil ((log (float (Int.int_of q))) /. (log 2.0))))
          (Params.qs Sec.kappa)
    ; r = Params.r Sec.kappa }

  let paramsi i : Ops.params =
    { phi = params.phi ; n = params.n; q = params.qs.(i); logq_ceil = params.logq_ceils.(i); r = params.r}

  type pse_pair =
    { pk : Poly.t; sk : Poly.t array; ek : (Poly.t array * Poly.t array) array }

  let compute_keypair params f g =
    let f_inv = invert params f in
    let h = center_lift params (Poly.scale (Int.of_int 2) (mul_star params g f_inv)) in
    { pk = h; sk = f }

  let rec pksk_gen i _ =
    let params = paramsi i in
    let f' = sample params () in
    let g  = sample params () in
    let f  = add params (Poly.scale (Int.of_int 2) f') Poly.one in
    try compute_keypair params f g
    with _ -> pksk_gen i ()

  (** generate d-many pairs of (pk, sk) *)
  let rec pksk_vec_gen d =
    Array.init d (fun i -> pksk_gen i ())

  let rec keygen _ =
    let pksk_vec = pksk_vec_gen (params.d + 1) in
    let sample_gamma i =
      sample_vec (paramsi (i + 1)) params.logq_ceils.(i + 1)
    in
    let ss_gamma_vec = Array.init params.d sample_gamma in
    let es_gamma_vec = Array.init params.d sample_gamma in
    let ss_zeta_vec = Array.init params.d sample_gamma in
    let es_zeta_vec = Array.init params.d sample_gamma in
    let init_gamma i =
      let params = paramsi (i + 1) in
      (** h^(i) s^(i) + 2 e^(i) + Pow(f^(i-1)), i = 1...D  *)
      (** h^(i + 1) s^(i + 1) + 2 e^(i + 1) + Pow(f^(i)), i = 0..(D-1)  *)
      let pow_i = pow params pksk_vec.(i).sk in
      let hs = vec_scale params pksk_vec.(i + 1).pk ss_gamma_vec.(i) in
      let two_e_i = vec_scale params (Poly.of_int 2) es_gamma_vec.(i) in
      Array.map (center_lift params)
                (vec_add params (vec_add params hs two_e_i) pow_i)
    in

    (** for i in [D] *)
    let gamma = Array.init params.d init_gamma in
    let zeta = Array.init params.d
        (fun i ->
           let params = paramsi (i + 1) in
           Array.map (center_lift params)
             (vec_add params
                (vec_add params
                   (vec_scale params pksk_vec.(i + 1).pk ss_zeta_vec.(i))
                   (vec_scale params (Poly.of_int 2) es_zeta_vec.(i)))
                (pow params (mul_star params pksk_vec.(i).sk pksk_vec.(i).sk)))) in
    let ek = BatArray.map2 (fun gamma zeta -> (gamma, zeta)) gamma zeta in
    let sk = Array.init (params.d) (fun i -> pksk_vec.(i).sk) in
    { pk = pksk_vec.(0).pk; sk = sk; ek = ek }

  let rec encrypt pk m =
    let param = paramsi 0 in
    let s = sample param () in
    let e = sample param () in
    center_lift param
      (add param (mul_star param pk s)
         (add param (Poly.scale (Int.of_int 2) e) (Poly.of_int m)))

  let decrypt i sk c =
    let param = paramsi i in
    let mu = center_lift param (mul_star param sk.(i) c) in
    mod2 mu

  (** Split a polynomial p into an array of width polynomials p_i with
      coefficients in {0, 1} such that summing 2^i p_i is equal to p. *)
  let slice p width =
    let coefficient_bits = Array.map (Int.bits width) p in
    let bit i =
      Poly.make (Array.map (fun c_bits -> c_bits.(i)) coefficient_bits)
    in
    Array.init width bit

  let log2 x =
    log x /. log 2.0

  let relinearize zetas level ciphertext =
    let param_cur = paramsi level in
    let bitwidth_float = (log2 (float_of_int (Int.int_of param_cur.q))) in
    let bitwidth = int_of_float (ceil bitwidth_float) in
    let sliced = slice ciphertext bitwidth in
    let zeta = zetas.(level + 1) in
    Ops.dot_mod param_cur zeta sliced
end
