open Arg
open List

open NTRULHE

let args =
  let ntru_rank = ref 0 in
  let ntru_p = ref 0 in
  let alpha = ref 0 in
  let mu = ref 0 in
  let file = ref "/dev/stdin" in
  let specs =
    [("-rank", Set_int ntru_rank, "degree N for the modulus phi = x^N - 1");
     ("-ntru-p", Set_int ntru_p, "lower bound on the prime modulus p");
     ("-alpha", Set_int alpha, "additive level");
     ("-mu", Set_int mu, "multiplicative level")
    ]
  in
  parse specs (fun f -> file := f) "NTRU parameter selection demo";
  let p = Int.nextprime (Int.of_int (!ntru_p - 1)) in
  let ntru_params = generate_params_for_level !alpha !mu !ntru_rank p in
  Printf.printf "generated params:\n%s\nalpha = %d\nmu = %d\n"
                (show_params ntru_params)
                !alpha
                !mu
