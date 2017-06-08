open OUnit2
open TestLib
open NTRUEncrypt
open NTRULHE
open BookNTRULHE

let plaintexts_for_little_params =
  [Polynomial.one;
   Polynomial.zero;
   TestPolynomial.make' [1; 0; 0; 1; 0; 1];
   TestPolynomial.make' [1; 0; 1; 0; 0; 1; 12];
   TestPolynomial.make' [1; 0; 1; 0; 1; 1; 12];
   TestPolynomial.make' [12; 3; 1; 0; 2; 1]]

(* Use little parameters with a huge enough q that we get alpha = 1, mu = 2 *)
let params = generate_params_for_level 1 2
               (Polynomial.deg little_ntru_lhe_params.phi)
               (Int.of_int 13)

let test_addition keypair plaintext_pairs =
  let check_addition_eq (m1, m2) =
    let c1 = encrypt params keypair.pubkey m1 in
    let c2 = encrypt params keypair.pubkey m2 in
    let d12 = decrypt params keypair (Polynomial.add c1 c2) in
    TestPolynomial.assert_poly_equal (Polynomial.add m1 m2) d12
  in
  List.iter check_addition_eq (List.map (fun (x,y) -> (center_lift_p params x, center_lift_p params y)) plaintext_pairs)

let test_multiplication keypair plaintext_pairs =
  let check_multiplication_eq (m1, m2) =
    let c1 = encrypt params keypair.pubkey m1 in
    let c2 = encrypt params keypair.pubkey m2 in
    let m12 = mul_star params m1 m2 in
    let m122 = mul_star params m12 m2 in
    let c12 = mul_star params c1 c2 in
    let c122 = mul_star params c12 c2 in
    let f2 = pow_star params 2 keypair.privkey_f in
    let f3 = pow_star params 3 keypair.privkey_f in
    let d12 = decrypt params {keypair with privkey_f = f2} c12 in
    let d122 = decrypt params {keypair with privkey_f = f3} c122
    in
    TestPolynomial.assert_poly_equal (center_lift_p params m12) d12;
    TestPolynomial.assert_poly_equal (center_lift_p params m122) d122;
  in
  List.iter check_multiplication_eq (List.map (fun (x,y) -> (center_lift_p params x, center_lift_p params y)) plaintext_pairs)

let test_keygen_inverse_and_encryption seed ctx =
  (* keep tests deterministic, hopefully *)
  Random.init seed;
  BatRandom.init seed;
  let keys = keygen params in
  let ff = mul_star_modp params keys.privkey_f keys.privkey_f_invp in
  let f2 = mul_star_modp params keys.privkey_f keys.privkey_f in
  let plaintext_pairs = BatList.cartesian_product plaintexts_for_little_params
                                                  plaintexts_for_little_params
  in
  logf ctx `Info "f = %s\nf_invp = %s\nf * f = %s mod phi\nf * f_invp = %s mod phi\n"
       (Poly.string_of keys.privkey_f)
       (Poly.string_of keys.privkey_f_invp)
       (Poly.string_of f2)
       (Poly.string_of ff);
  TestPolynomial.assert_poly_equal Polynomial.one (center_lift_p params ff);
  test_addition keys plaintext_pairs;
  test_multiplication keys plaintext_pairs

let make_test_with_seed s =
  let name = Printf.sprintf "test_keygen_inverse_and_encryption %d" s in
  name >:: test_keygen_inverse_and_encryption s

let seeds = [1; 90; 123; 567; 9999; -100]

let suite : test =
  "TestNTRULHE" >:::
    List.map make_test_with_seed seeds
