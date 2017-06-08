open Arg
open List

open NTRULHE

let read_polys_from_file filename =
  let polys = ref [] in
  let comma = Str.regexp "," in
  let poly_of_line l =
    let int_coes = List.map int_of_string (Str.split comma l) in
    let real_coes = List.map Int.of_int int_coes in
    Poly.make (Array.of_list real_coes)
  in
  let chan = open_in filename in
  try while true; do
        let l = input_line chan in
        polys := (poly_of_line l) :: !polys
      done;
      List.rev !polys
  with End_of_file ->
    close_in chan;
    List.rev !polys

let check_polys_eq p q =
  if not (Poly.eq p q)
  then Printf.printf "%s != %s\n" (Poly.string_of p) (Poly.string_of q)


let args =
  let ntru_rank = ref 0 in
  let ntru_p = ref 0 in
  let ntru_q = ref 0 in
  let ntru_d = ref 0 in
  let file = ref "/dev/stdin" in
  let specs =
    [("-rank", Set_int ntru_rank, "degree N for the modulus phi = x^N - 1");
     ("-ntru-p", Set_int ntru_p, "prime modulus p for NTRU");
     ("-ntru-q", Set_int ntru_q, "modulus q for NTRU");
     ("-ntru-d", Set_int ntru_d, "integer d for key size")]
  in
  parse specs (fun f -> file := f) "";

  let module LHEParams = struct
    let phi = Poly.sub (Poly.make_monomial Int.one !ntru_rank) Poly.one
    let p = Int.of_int !ntru_p
    let q = Int.of_int !ntru_q
    let d = !ntru_d
  end in
  let module Encrypt = BookNTRULHE(LHEParams) in
  let open Encrypt in

  let test_multiplication keypair plaintext_pairs =
    let check_multiplication_eq (m1, m2) =
      let c1 = encrypt keypair.pubkey m1 in
      let c2 = encrypt keypair.pubkey m2 in
      let d12 = decrypt
                  { keypair with privkey_f = (mul_star keypair.privkey_f keypair.privkey_f) }
                  (mul_star c1 c2)
      in
      check_polys_eq (center_lift_p (mul_star m1 m2)) d12
    in
    List.iter check_multiplication_eq
              (List.map
                 (fun (x,y) ->
                   (center_lift_p x, center_lift_p y))
                 plaintext_pairs)
  in

  let ps = read_polys_from_file !file in
  List.iter2 check_polys_eq ps TestNTRULHE.plaintexts_for_little_params;
  let table = BatList.cartesian_product ps ps in
  Random.init 90;
  BatRandom.init 90;
  let keypair = Encrypt.keygen () in
  test_multiplication keypair table;
  let ps = map center_lift_p ps in
  let encs = map (encrypt keypair.pubkey) ps in
  let enc_five = encrypt keypair.pubkey Poly.one in
  let psm5 = map (mul_star Poly.one) ps in
  let encsm5 = map (mul_star enc_five) encs in
  let decs = map (decrypt keypair) encs in
  let decsm5 = map (decrypt ?level:(Some 2) keypair) encsm5 in
  List.iter2 check_polys_eq (map center_lift_p psm5) decsm5;
  List.iter2 check_polys_eq (map center_lift_p ps) (map center_lift_p decs)
