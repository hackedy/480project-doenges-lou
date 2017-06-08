(** Polynomials in Z[x] and its quotients. *)
open Ring

type coefficient = Int.t

(** A polynomial is represented by an array of its coefficients, so that if [f =
Poly p] then its constant term is [p.(0)], its linear coefficient is [p.(1)],
and so on. A polynomial's array always has [p.(Array.length p - 1)] <> 0], so
that we can use [Array.length] to compute the degree of the polynomial. *)
type t = Poly of coefficient array

(** Trim trailing zero coefficients off of the coefficient list [l]. *)
let trim l =
  let rec countz l n =
    if l.(n) != Int.zero || n = 0
    then n
    else countz l (n - 1)
  in
  Array.sub l 0 (countz l (Array.length l - 1) + 1)

(** Produce the constant polynomial corresponding to the given [Int.t]. *)
let const i =
  Poly [|i|]

(** Produce the constant polynomial corresponding to the given [int]. *)
let of_int i =
  const (Int.of_int i)

(** Extract the coefficients of the given polynomial as an array. *)
let array_of (Poly p) = Array.copy p

(** The zero polynomial. *)
let zero = of_int 0

(** The unit polynomial. *)
let one = of_int 1

(** Create a polynomial from an array of coefficients [|a_0; ...; a_n|]. If the
    list is empty, return the zero polynomial. *)
let make arr =
  if Array.length arr = 0
  then zero
  else Poly (trim arr)

(** Test two polynomials for equality. *)
let eq (Poly p_arr) (Poly q_arr) =
  make p_arr = make q_arr

(** Determine the degree of the given polynomial. *)
let deg (Poly p_arr) =
  let d = Array.length p_arr - 1 in
  assert (d = 0 || (p_arr.(d) <> Int.zero));
  d

(** Negate the given polynomial. *)
let neg (Poly p_arr) =
  Poly (Array.map Int.neg p_arr)

(** (Internal function.) Create an array of the numbers i, i + 1, ..., j - 1. If
    j [<=] i, returns the empty array. *)
let mk_range i j : int array =
  if j <= i
  then Array.make 0 0
  else Array.init (j - i) (fun k -> i + k)

(** Get the ith coefficient of the polynomial. *)
let coeff (Poly p_arr) i =
  if i < Array.length p_arr
  then p_arr.(i)
  else Int.zero

(** Add the polynomials p and q. *)
let add p q : t =
  let degree = max (deg p) (deg q) in
  let coe i = Int.add (coeff p i) (coeff q i) in
  make (Array.init (degree + 1) coe)

(** Subtract the polynomial p from q. *)
let sub p q =
  add p (neg q)

(** Multiply the polynomials p and q as elements of Z[x]. *)
let mul p q : t =
  let conv i j = Int.mul (coeff p j) (coeff q (i - j)) in
  let coeff_terms i = Array.map (conv i) (mk_range 0 (i + 1)) in
  let coeff i = Array.fold_left Int.add Int.zero (coeff_terms i) in
  (* as in (add p q), we might have some zeroes at the high end of the array
     which we should trim off. *)
  make (Array.init (deg p + deg q + 1) coeff)

(** Produce a human-readable representation of the polynomial p. *)
let string_of (Poly p_arr) : string =
  let string_of_monomial i ai =
    match i with
    | 0 -> Int.string_of ai
    | 1 -> Int.string_of ai ^ " x"
    | n -> Int.string_of ai ^ " x^" ^ string_of_int n
  in
  let monos = Array.mapi string_of_monomial p_arr in
  String.concat " + " (Array.to_list monos)

(** Compute p(x) for the given integer x. *)
let subst p x : Int.t =
  let subst_mono i c_i = Int.mul c_i (Int.pow x i) in
  Array.fold_left Int.add Int.zero (Array.mapi subst_mono p)
(** Evaluate the polynomial f at point x *)
let rec evaluate (Poly f) x =
  Array.fold_left Int.add Int.zero
  (Array.mapi (fun i a -> (Int.mul (Int.pow x i) a)) f)

(** Check if the polynomial is monic. *)
let is_monic (Poly p_arr) =
  let l = Array.length p_arr in
  Int.eq p_arr.(l - 1) Int.one

(** Multiply the polynomial by the scalar i. *)
let scale i (Poly p_arr) =
  Poly (Array.map (Int.mul i) p_arr)

(** Make the monomial coefficient*x^power. *)
let make_monomial coefficient power =
  let arr = Array.make (power + 1) Int.zero in
  arr.(power) <- coefficient;
  make arr

(** Extract the nth monomial of the given polynomial. *)
let nth_monomial p n =
  let coe i =
    if i = n
    then coeff p n
    else Int.zero
  in
  make (Array.init (n + 1) coe)

(** Map the given function over the cofficients of the polynomial and return the
    resulting polynomial. *)
let map_coeff f (Poly p) =
  make (Array.map f p)

(** [mod_coeff p f] reduces the coefficients of f modulo p. *)
let mod_coeff p f =
  map_coeff (fun i -> Int.modulo i p) f

(** Center-lift the coefficients of [p] modulo [modulus]. *)
let center_lift modulus p =
  map_coeff (Int.center_lift modulus) p

(** Given [f] and [g],
    return ([q], [r]) such that [f = qg + r] and [deg r < deg g]. *)
let rec div_rem f g =
  let deg_f = deg f in
  let deg_g = deg g in
  if deg_f < deg_g
  then ( zero , f )
  else if deg_f = deg_g && deg_f = 0
  then (make [|Int.mul (coeff f 0) (Int.inv (coeff g 0))|] , zero )
  else
    let leadCoeff = Int.mul (coeff f deg_f) (Int.inv (coeff g deg_g))
    in
    let leadPoly = make (Array.init (deg_f - deg_g + 1) (fun i -> if i = deg_f - deg_g then leadCoeff else Int.zero)) in
    let f' = sub f (mul g leadPoly) in
    let (q' , r') = div_rem f' g
    in ( add q' leadPoly , r' )

(** Given [f] and [g], return ([q], [r]) such that [f = qg + r] in Z/pZ[x], with
    [deg r < deg g]. *)
let rec div_rem_mod p f g =
  let deg_f = deg f in
  let deg_g = deg g in
  if deg_f < deg_g
  then (zero, f)
  else
    let f_n = coeff f deg_f in
    let g_m = coeff g deg_g in
    let lead_coeff = Int.modulo (Int.mul f_n (Int.inv_mod p g_m)) p in
    if deg_f = 0 && deg_g = 0
    then (const lead_coeff, zero)
    else
      let lead_poly = make_monomial lead_coeff (deg_f - deg_g) in
      let f' = mod_coeff p (sub f (mul g lead_poly)) in
      let (q', r') = div_rem_mod p f' g in
      (mod_coeff p (add q' lead_poly), mod_coeff p r')

(** [mul_star phi f g] multiplies [f] and [g] in [Z[x]/<phi>]. *)
let mul_star phi f g =
  let (_, rem) = div_rem (mul f g) phi in
  rem

(** [mul_star_mod phi p f g] multiplies [f] and [g] in [Z/pZ[x]/<phi>]. *)
let mul_star_mod phi modulus f g =
  let (_, rem) = div_rem_mod modulus (mul f g) phi in
  rem

(** Run the extended Euclidean algorithm on [a] and [b] and return [(s, t, g)]
    such that [g] is a gcd of [a] and [b] and [as + tb = g]. *)
let rec eea a b : t * t * t =
  if b = zero
  then (one, zero, a)
  else
    let (q,r) = div_rem a b in
    let (s,t,g) = eea b r in
    (t, sub s (mul q t), g)

(** Add the polynomials [a] and [b] in the ring [Z/pZ[x]]. *)
let add_mod p a b =
  mod_coeff p (add a b)

(* Run the extended Euclidean algorithm on [a] and [b] and return [(s, t, g)]
   such that [g] is a gcd of [a] and [b] in R and [as + tb = g] in R, where R is
   the ring [Z/pZ[x]]. *)
let rec eea_mod p a b : t * t * t =
  let a = mod_coeff p a in
  let b = mod_coeff p b in
  if eq b zero
  then (one, zero, a)
  else
    let (q, r) = div_rem_mod p a b in
    let (s, t, g) = eea_mod p b r in
    (mod_coeff p t, mod_coeff p (sub s (mul q t)), mod_coeff p g)

(** Running[random_trit_poly_with_counts length count_ones count_neg_ones]
    samples a polynomial from the uniform distribution T([count_ones],
    [count_neg_ones]) with a maximum polynomial degree of [length]. *)
let random_trit_poly_with_counts length count_ones count_neg_ones =
  let zeros = length - count_ones - count_neg_ones in
  let scramble_this = Array.concat [Array.make count_ones Int.one;
                                    Array.make count_neg_ones (Int.neg Int.one);
                                    Array.make zeros Int.zero]
  in
  make (BatRandom.shuffle (BatArray.enum scramble_this))

(** Find an inverse for f in the ring R = [Z/pZ[x]]. *)
let invert_mod phi p f : t =
  let (a, b, g) = eea_mod p f phi in
  if deg g = 0
  then let g0 = coeff g 0 in
       let ginv = Int.inv_mod p g0 in
       mod_coeff p (scale ginv a)
  (* todo: use a special exception *)
  else invalid_arg "non-invertible"

(** Compute the [n]th power of the polynomial [x]. *)
let rec pow x n =
  if n < 0
  then invalid_arg "cannot raise a polynomial to a negative power"
  else if n = 0
  then one
  else mul x (pow x (n - 1))

(* Compute an array of bit-arrays for each coefficient. *)
let coeff_bits width (Poly f) =
  Array.map (Int.bits width) f

(** Given integers [a] and [b], compute [a/b * f] and then round it to the
    closest polynomial g such that f = g modulo 2. *)
let rounded_division_mod2 a b f =
  map_coeff (Int.rounded_division_mod2 a b) f
