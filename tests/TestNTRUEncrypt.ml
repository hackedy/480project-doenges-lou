open OUnit2
open TestLib
open NTRUEncrypt
open BookNTRU

let plaintexts_for_little_params =
  [Polynomial.one;
   Polynomial.zero;
   TestPolynomial.make' [1; 0; 0; 1; 0; 1];
   TestPolynomial.make' [1; 0; 1; 0; 0; 1; 12];
   TestPolynomial.make' [12; 3; 1; 0; 2; 1]]

let params = little_ntru_params

let test_encrypt_decrypt keypair plaintexts =
  let check_roundtrip plaintext =
    let ciphertext = encrypt params keypair.pubkey plaintext in
    let decrypted = decrypt params keypair ciphertext in
    let msg = Printf.sprintf
                "ciphertext is equal to plaintext!\nplaintext = %s\nciphertext = %s\n"
                (Polynomial.string_of plaintext)
                (Polynomial.string_of ciphertext)
    in
    assert_bool msg (not (Polynomial.eq plaintext ciphertext));
    TestPolynomial.assert_poly_equal plaintext decrypted
  in
  List.iter check_roundtrip (List.map (center_lift_p params) plaintexts)

let test_keygen_inverse_and_encryption seed ctx =
  (* keep tests deterministic, hopefully *)
  Random.init seed;
  BatRandom.init seed;
  let keys = keygen params in
  let ff = mul_star_modp params keys.privkey_f keys.privkey_f_invp in
  logf ctx `Info "f = %s\nf_invp = %s\nf * f_invp = %s mod phi\n"
       (Poly.string_of keys.privkey_f)
       (Poly.string_of keys.privkey_f_invp)
       (Poly.string_of ff);
  TestPolynomial.assert_poly_equal Polynomial.one (center_lift_p params ff);
  test_encrypt_decrypt keys plaintexts_for_little_params

let make_test_with_seed s =
  let name = Printf.sprintf "test_keygen_inverse_and_encryption %d" s in
  name >:: test_keygen_inverse_and_encryption s

let seeds = [1; 90; 123; 567; 9999; -100]

let suite : test =
  "TestNTRUEncrypt" >:::
    List.map make_test_with_seed seeds
