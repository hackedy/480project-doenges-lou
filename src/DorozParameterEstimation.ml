let two = Z.of_int 2

let doroz_figure_1 =
  [ (12, 155, 43, 2, 27, 4)
  ; (13, 311, 46, 5, 29, 9)
  ; (14, 622, 49, 11, 30, 19)
  ; (15, 1244, 52, 22, 32, 37)
  ; (16, 2488, 55, 44, 33, 74)
  ; (17, 4976, 58, 84, 35, 141)
  ]

type doroz_parameters =
  { (* Chi produces b-bounded polynomials. *)
    b : Z.t
  ; alpha : Z.t
  ; log_n : Z.t
  ; log_q : Z.t
  }

let compute_q_given_n_and_B n b =
  let b_poly = Z.(~$64 * b**4 + ~$48 * b**3 + ~$9 * b**2) in
  let lower_bound = Z.(n**3 * b_poly) in
  let ans = Z.log2(lower_bound) + 1 in
  Z.of_int ans

let invlog2 = 1.0 /. log 2.0
let log2 x =
  log x *. invlog2

(* The work factor delta^2n = sqrt(q) / 4 *)
let compute_hermite k n logq =
  (* we compute the work factor by first computing
         log(delta) = 1/2n (1/2 * log q - log 4)
     and then exponentiating it. *)
  let logsqrtq4 = 0.5 *. logq -. log2 k in
  let factor = 0.5 /. n in
  2.0 ** (factor *. logsqrtq4)

let estimate_kappa epsilon alpha additions dimension b logq =
  let n = float_of_int dimension in
  let a = float_of_int additions in
  let b = float_of_int b in
  let t1 = a *. n *. (6.0*.b*.b +. 2.0*.b) *. (logq +. log(a)) in
  let bb1 = 2.0*.b +. 1.0 in
  let t2 = n**1.5 *. bb1*.bb1 *. b*.b in
  (t1 +. t2) *. epsilon

let next_bound kappa alpha additions dimension b logq prev_bound =
  let n = float_of_int dimension in
  let a = float_of_int additions in
  let b = float_of_int b in
  let t1 = a *. n *. (6.0*.b*.b +. 2.0*.b) *. (logq +. log(a)) in
  let bb1 = 2.0*.b +. 1.0 in
  let t2 = n**1.5 *. bb1*.bb1 *. prev_bound*.prev_bound in
  (t1 +. t2) *. kappa

let pl _ =
  Printf.printf "\n";
  flush stdout

let pz z =
  Printf.printf "%s" (Z.to_string z)

let _ =
  let n = Z.pow two 13 in
  let b = two in
  let q = compute_q_given_n_and_B n b in
  let h = compute_hermite 4.0 (Z.to_float n) 512.0 in
  Printf.printf "%.4f\n" h;
  pz q;
  pl
