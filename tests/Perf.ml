open TestLib
open NTRULHE

module LittleLHENTRU = BookNTRULHE(LittleLHEParams)
open LittleLHENTRU

let plaintexts_for_little_params =
  [Polynomial.one;
   Polynomial.zero;
   TestPolynomial.make' [1; 0; 0; 1; 0; 1];
   TestPolynomial.make' [1; 0; 1; 0; 0; 1; 12];
   TestPolynomial.make' [1; 0; 1; 0; 1; 1; 12];
   TestPolynomial.make' [12; 3; 1; 0; 2; 1]]


let run_addition keypair plaintext_pairs =
  let check_addition_eq (m1, m2) =
    let c1 = encrypt keypair.pubkey m1 in
    let c2 = encrypt keypair.pubkey m2 in
    let d12 = decrypt keypair (Polynomial.add c1 c2) in
    if d12 = c2 then () else failwith "not equal"
  in
  List.iter check_addition_eq (List.map (fun (x,y) -> (center_lift_p x, center_lift_p y)) plaintext_pairs)

let test_multiplication keypair plaintext_pairs =
  let check_multiplication_eq (m1, m2) =
    let c1 = encrypt keypair.pubkey m1 in
    let c2 = encrypt keypair.pubkey m2 in
    let d12 = decrypt
        { keypair with privkey_f = (mul_star keypair.privkey_f keypair.privkey_f) }
        (mul_star c1 c2)
    in
    if center_lift_p (mul_star m1 m2) = d12
    then ()
    else failwith "not equal"
  in
  List.iter check_multiplication_eq (List.map (fun (x,y) -> (center_lift_p x, center_lift_p y)) plaintext_pairs)


let test_keygen_inverse_and_encryption seed =
  (* keep tests deterministic, hopefully *)
  Random.init seed;
  BatRandom.init seed;
  let keys = keygen () in
  let ff = mul_star_modp keys.privkey_f keys.privkey_f_invp in
  assert (Polynomial.one = (center_lift_p ff));
  run_addition keys (List.combine plaintexts_for_little_params plaintexts_for_little_params) ;
  test_multiplication keys (List.combine plaintexts_for_little_params plaintexts_for_little_params)

let seeds = [1; 90; 123; 567; 9999; -100]

let _ =
  List.iter test_keygen_inverse_and_encryption seeds
