open OUnit2
open OUnitTest
open TestLib
open Polynomial

let assert_poly_equal p q =
  assert_equal ~printer:string_of p q

let polynomials = [|
    [|5; 17; 10; 1; 0; 4; 0; 19; 1; 2|];
    [|13; 9; 9; 8; 6; 1; 12; 15; 18; 9|];
    [|1|];
    [|0|];
    [|1; 0; 0; 0; 0; -1; 12|];
    [|9; -10; -7; -7; -10; 7; 1; 5|]
  |]

let make' coefficients =
  make (Array.of_list (List.map Int.of_int coefficients))

let make_table ps qs f =
  let mk_row i = Array.init (Array.length qs) (f i) in
  Array.init (Array.length ps) mk_row

type div_rem_mod_testcase = {
    modulus : Int.t;
    dividend : t;
    divisor : t;
    quotient : t;
    remainder : t
  }

(* got these polynomials from https://math.stackexchange.com/questions/124300/finding-inverse-of-polynomial-in-a-field *)
let se_f = make' [1; 0; 1]
let se_phi = make' [1; 2; 0; 1]
let se_p = Int.of_int 3
let se_gcd = one

let stackexchange_case = {
    modulus = se_p;
    dividend = se_phi;
    divisor = se_f;
    quotient = make_monomial Int.one 1;
    remainder = make' [1; 1]
  }

let intermediate_case_of_eea_on_se = {
    modulus = se_p;
    dividend = make' [1; 1];
    divisor = make' [2];
    quotient = make' [2; 2];
    remainder = make' [0]
  }

let check_div_rem_mod_correct case =
  let (q, r) = div_rem_mod case.modulus case.dividend case.divisor in
  let qg_r = (mod_coeff case.modulus (add (mul q case.divisor) r)) in
  assert_poly_equal case.dividend qg_r;
  (q, r)

let test_div_rem_mod case _ =
  let (q, r) = check_div_rem_mod_correct case in
  assert_poly_equal case.quotient q;
  assert_poly_equal case.remainder r

let div_rem_mod_tests =
  let make_test case =
    test_case ?length:(Some Immediate) (test_div_rem_mod case)
  in
  List.map make_test [stackexchange_case; intermediate_case_of_eea_on_se]

let check_eea_mod_valid p x y a b g =
  let ax_by = (add (mul a x) (mul b y)) in
  assert_poly_equal g (mod_coeff p ax_by)

let test_eea_mod _ =
  let (a, b, g) = eea_mod se_p se_phi se_f in
  check_eea_mod_valid se_p se_phi se_f a b g;
  assert_equal 0 (deg g)

let test_invert_mod _ =
  let h = invert_mod se_phi se_p se_f in
  assert_poly_equal (make' [2; 1; 2]) h

let suite : test =
  "Polynomial" >:::
    ["div_rem_mod" >::: div_rem_mod_tests;
     quick_test "test_eea_mod" test_eea_mod;
     quick_test "test_invert_mod" test_invert_mod]
