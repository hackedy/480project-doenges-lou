open OUnit2
open Ring

module Rank = struct let n = 5 end
module CPZ5 = ConvPoly(Z)(Polynomial(Z)(Rank))(Rank)

let test_one_squared _ =
  assert_equal (CPZ5.mul CPZ5.one CPZ5.one) CPZ5.one

let test_zero_plus_zero _ =
  assert_equal (CPZ5.add CPZ5.zero CPZ5.zero) CPZ5.zero

let suite : test =
  "convpoly_tests">:::
    ["test 1*1=1">:: test_one_squared;
     "test 0+0=0">:: test_zero_plus_zero]

