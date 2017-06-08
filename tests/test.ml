open OUnit2

let all_tests =
  "all_tests"
  >::: [TestLTV.suite;
        TestNTRULHE.suite;
        TestNTRUEncrypt.suite;
        (*TestDoroz.suite;*)
        TestPolynomial.suite;
        TestInt.suite]

let _ =
    run_test_tt_main all_tests
