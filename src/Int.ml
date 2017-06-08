type t = Z.t

let of_int i = Z.of_int i

let int_of n = Z.to_int n

let string_of i = Z.to_string i

let zero = of_int 0

let one = of_int 1

let two = of_int 2

let add a b = Z.add a b

let sub a b = Z.sub a b

let mul a b = Z.mul a b

let neg a = Z.neg a

let leq a b = Z.leq a b

let geq a b = Z.geq a b

let modulo a b =
  let m = Z.rem a b in
  if geq m zero
  then m
  else add b m

let eq i j =
  i = j

let inv n =
  if eq n one || eq n (neg one)
  then n
  else invalid_arg "-1 and 1 are the only units in Z"

let lt a b = Z.lt a b

let gt a b = Z.gt a b

let rec pow x n =
  if n < 0
  then invalid_arg "cannot raise an Int.t to a negative power"
  else if n = 0
  then one
  else mul x (pow x (n - 1))

let div_rem a b =
  Z.div_rem a b

let rec gcd a b =
  Z.gcd a b

let rec eea a b : t * t * t =
  let (g, s, t) = Z.gcdext a b in
  (s, t, g)

let inv_mod p a =
  Z.invert a p

let div a b =
  Z.div a b

let rec center_lift modulus a =
  let a' = modulo a modulus in
  if gt a' (div modulus (of_int 2))
  then sub a' modulus
  else a'

let random bound =
  of_int (Random.int (int_of bound))

let abs k =
  Z.abs k

let shift_right (k : t) (w : int) : t =
  Z.shift_right k w

let rec bitwidth n =
  Z.numbits n

let bits width n =
  let bits_pos n =
    let bit i =
      modulo (shift_right n i) two
    in
    Array.init width bit
  in
  if geq n zero
  then bits_pos n
  else Array.map neg (bits_pos (neg n))

(* compute e = a / b * x and then round e to the nearest integer such that e
   \equiv x mod 2. *)
let rounded_division_mod2 a b x =
  let go x =
    let mod2 n = modulo n two in
    let x_parity = mod2 x in
    let ax = mul a x in
    let (q, r) = div_rem ax b in
    if eq (mod2 q) x_parity
    then q
    else let better = add one q in
         assert (eq (mod2 better) x_parity);
         better
  in
  if geq x zero
  then go x
  else neg (go (neg x))

(** Find the next prime larger than k. The underlying algorithm may sometimes
    return a composite number. *)
let nextprime k =
  Z.nextprime k

(** Compute a prime number with at least the given number of bits. *)
let primebits bits =
  let lower_bound = Z.(~$2 ** bits - ~$1) in
  nextprime lower_bound
