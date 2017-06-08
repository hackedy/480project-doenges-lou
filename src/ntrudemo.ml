open Arg
open List

open NTRULHE
open BookNTRULHE
open NTRUEncrypt

let args =
  Random.self_init ();
  let ntru_rank = ref 11 in
  let ntru_p = ref 13 in
  let alpha = ref 1 in
  let mu = ref 10 in
  let poly_count = ref 10 in
  let mul_probability = ref 0.1 in
  let specs =
    [("-rank", Set_int ntru_rank, "degree N for the modulus phi = x^N - 1 [default: 11]");
     ("-ntru-p", Set_int ntru_p, "lower bound on the prime modulus p [default: 13]");
     ("-alpha", Set_int alpha, "desired additive level [default: 1]");
     ("-mu", Set_int mu, "desired multiplicative level [default: 10]");
     ("-polys", Set_int poly_count, "number of polynomials to seed with [default: 10]");
     ("-mix", Set_float mul_probability, "probability in 0.0 to 1.0 of a multiplication happening [default: 0.1]")
    ]
  in
  parse specs (fun _ -> ()) "NTRU demo";
  let n = !ntru_rank in
  let p = Int.nextprime (Int.of_int (!ntru_p - 1)) in
  Printf.printf "generating q...\n";
  let ntru_params = generate_params_for_level !alpha !mu n p in
  Printf.printf "generated q = %s\n" (Int.string_of ntru_params.q);
  let random_poly _ =
    RandomPolynomials.sample_bounded_poly n (Int.int_of p)
  in
  let count = !poly_count in
  let polys = Array.init count random_poly in
  let print_p i p =
    Printf.printf "m_%d = %s\n" i (Poly.string_of p)
  in
  Array.iteri print_p polys;

  let keys = keygen ntru_params in
  let enc m = encrypt ntru_params keys.pubkey m in
  let dec k c = decrypt ntru_params keys ~level:k c in
  let ciphertexts = Array.map enc polys in
  let mul = mul_star ntru_params in

  let rec step_game plaintexts ciphertexts multiplications round =
    let check_decryption i m =
      let m' = dec (multiplications.(i) + 1) ciphertexts.(i) in
      let mp = center_lift_p ntru_params m in
      if m' <> mp
      then
        begin
          let mults = multiplications.(i) in
          Printf.printf "after %d multiplications,\ndecryption gave: %s\nbut we expected: %s\n"
                        mults (Poly.string_of m') (Poly.string_of mp);
          if mults >= !mu
          then exit 0
          else exit 1
        end
    in

    Printf.printf "checking that decryption still works...\n";
    Array.iteri check_decryption plaintexts;
    let threshold = !mul_probability in

    let plaintexts' = Array.copy plaintexts in
    let ciphertexts' = Array.copy plaintexts in
    let multiplications' = Array.copy multiplications in
    let first_mul = ref true in
    let find_and_mult i _ =
      if Random.float 1.0 > threshold
      then ()
      else
        let j = Random.int count in
        begin
          if !first_mul
          then (first_mul := false; Printf.printf "mul %d %d" i j)
          else Printf.printf ", mul %d %d" i j
        end;
        plaintexts'.(i) <- mul plaintexts.(i) plaintexts.(j);
        ciphertexts'.(i) <- mul ciphertexts.(i) ciphertexts.(j);
        multiplications'.(i) <- multiplications.(i) + multiplications.(j) + 1;
    in

    Printf.printf "\nperforming round %d of random multiplications\n" round;
    Array.iteri find_and_mult plaintexts;
    if not !first_mul then Printf.printf ".\n";
    step_game plaintexts' ciphertexts' multiplications' (1 + round)
  in
  step_game polys ciphertexts (Array.make count 0) 1
