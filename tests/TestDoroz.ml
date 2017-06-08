open OUnit2
open Doroz

let little_qs =
  let q0 = Int.primebits 10 in
  let q1 = Int.primebits (2) in
  [|q0; q1|]

let params = make_public_params little_qs 100 1

let encrypt_decrypt_roundtrip pubkey privkey bit =
  let ciphertext = encrypt params pubkey bit in
  let decrypted = decrypt params privkey ciphertext in
  assert_equal decrypted bit;
  (decrypted, ciphertext)

let roundtrip_test _ =
  (* Give me 4 sets of keys please *)
  let keys = Array.init 4 (fun _ -> keygen params) in
  let validate_keys (sk, pk, ek) =
    let validate_ek i zeta =
      if i > 0
      then assert_equal ~printer:string_of_int params.bitwidths.(i) (Array.length zeta)
    in
    Array.iteri validate_ek ek
  in
  let encrypt_under_keys (sk, pk, ek) bit =
    encrypt params pk bit
  in
  let decrypt_and_check (sk, pk, ek) bit ciphertext =
    let d = decrypt params sk ciphertext in
    assert_equal bit d
  in
  let run_tests_with ks =
    validate_keys ks;
    let ct = encrypt_under_keys ks true in
    decrypt_and_check ks true ct;
    let (sk, pk, ek) = ks in
    let bumped_ct = mul params ek ct ct in
    let ct' =
      { level = ct.level
      ; text = Poly.add (Poly.add ct.text ct.text) ct.text
      }
    in
    let d = decrypt params sk ct' in
    assert_equal true d;
    let d = decrypt params sk bumped_ct in
    assert_equal true d
  in
  Array.iter run_tests_with keys

let suite : test =
  "TestDoroz" >::: [test_case roundtrip_test]
