open OUnit2
open Int

let twenty_primes =
  [3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73]

let three = of_int 3

let mk_field p =
  let l = BatList.range 0 `To (p - 1) in
  List.map of_int l

let check_inv_mod p x =
  let x_inv = inv_mod p x in
  let left = modulo (mul x x_inv) p in
  let right = modulo (mul x_inv x) p in
  assert_equal one left;
  assert_equal one right;
  x_inv

let test_inv_mod p _ =
  let p' = of_int p in
  let fp = mk_field p in
  let check x =
    if eq x zero then () else
    let x_inv = check_inv_mod p' x in
    let msg = Printf.sprintf "%s in F_%s" (string_of x) (string_of p') in
    assert_bool msg (lt x_inv p' && geq x_inv zero)
  in
  List.iter check fp

let test_inv_mods =
  let mk_test p =
    let name = Printf.sprintf "test_inv_mod%d" p in
    name >:: test_inv_mod p
  in
  List.map mk_test twenty_primes

let test_mul_mod _ =
  let h = modulo (mul one one) three in
  assert_equal ~printer:Int.string_of one h

let suite : test =
  "Int" >:::
    test_inv_mods @
    ["test_mul_mod"
     >:: test_mul_mod]
