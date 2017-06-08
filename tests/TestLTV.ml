open OUnit2
open TestLib
open LTV

(* module LittleLTVNTRU = LTVNTRU(LittleSecParam)(LittleLTVNTRUParams) *)
module LittleLTVNTRU = LHLTV(LittleSecParam)(LittleLHLTVParams)
(* module LittleLTVNTRU = SHLTV(LittleSecParam)(LittleLTVNTRUParams) *)
open LittleLTVNTRU

let plaintexts_for_little_params =
  [0;1;0;1;0;0;1;0;0;1;1;1;1;0;1;0;1;0;1;0;0;1;0;1;0;1;0;1;0;0;1;0;1;0;1;1;0;1;0;1;0;1]

let test_encrypt_decrypt keypair plaintexts =
  let check_roundtrip plaintext =
    let ciphertext = encrypt keypair.pk plaintext in
    (** does nothing to the ciphertext, we decrypt in level d *)
    let decrypted = decrypt 0 keypair.sk ciphertext in
    assert_equal plaintext decrypted;
    if plaintext != decrypted
    then assert_string ("plaintext: " ^ (string_of_int plaintext) ^ "  decrypted: " ^ (string_of_int decrypted));
    assert_equal plaintext decrypted
  in
  List.iter check_roundtrip plaintexts

let test_keygen_inverse_and_encryption seed ctx =
  (* keep tests deterministic, hopefully *)
  Random.init seed;
  BatRandom.init seed;
  let keys = keygen () in
  logf ctx `Info "pk = %s\nsk = %s\n"
       (Poly.string_of keys.pk)
       (Poly.string_of keys.sk.(0));
  test_encrypt_decrypt keys plaintexts_for_little_params

let make_test_with_seed s =
  let name = Printf.sprintf "test_keygen_inverse_and_encryption %d" s in
  name >:: test_keygen_inverse_and_encryption s

let seeds = [1; 90; 123; 567; 9999; -100]

let suite : test =
  "TestLTVNTRU" >:::
    List.map make_test_with_seed seeds
