open OUnit2
open TestLib
open RandomPolynomials

let gaussian_pdf_roundtrip _ =
  let std_dev = 35.0 in
  let epsilon = 0.5 in
  let roundtrip x =
    let y = gaussian_pdf std_dev x in
    let x' = gaussian_pdf_inverse std_dev y in
    (*Printf.printf "\nx =\t%f\ny =\t%f\nx' =\t%f\ndiff =\t%f\n" x y x' (abs_float (x' -. x)); *)
    assert_bool "error less than epsilon" (abs_float (x' -. x) < epsilon)
  in
  List.iter roundtrip [1.0; 145.0; 23.0; 49.0; 300.0; 0.05]

let check_sorted cmp arr =
  let len = Array.length arr in
  let rec go i =
    if i+1 < len
    then cmp arr.(i) arr.(i+1) && go (i+1)
    else true
  in
  go 0

let rec uniq =
  function
  | first :: second :: rest ->
     if first = second
     then uniq (first :: rest)
     else first :: uniq (second :: rest)
  | l -> l

let array_sorted cmp arr =
  let sorted = Array.copy arr in
  Array.sort cmp sorted;
  sorted

let uniq_sorted cmp arr =
  let sorted = array_sorted cmp arr in
  let sorted_list = Array.to_list sorted in
  let uniq_list = uniq sorted_list in
  Array.of_list uniq_list

let ziggurat_corners_sorted zigg =
  let compare' x y = -1 * compare x y in
  let xs = Array.map fst zigg.corners in
  let ys = Array.map snd zigg.corners in
  let xs_uniq = uniq_sorted compare xs in
  let ys_uniq = uniq_sorted compare' ys in
  assert_equal ~printer:(show_array string_of_int) xs xs_uniq;
  assert_equal ~printer:(show_array string_of_float) ys ys_uniq

let validate_ziggurat zigg =
  let corner_count = Array.length zigg.corners in
  assert_bool "more than 2 corners" (corner_count > 2);
  let (x_m, y_m) = zigg.corners.(corner_count - 1) in
  let (x_0, y_0) = zigg.corners.(0) in
  ziggurat_corners_sorted zigg;
  assert_bool "y_m = 0.0" (y_m = 0.0);
  let y_0errstring = Printf.sprintf "y_0 (%f) within 0.05 of 1.0" y_0 in
  assert_bool y_0errstring (abs_float (y_0 -. 1.0) < 0.05);
  assert_bool "x_0 = 0" (x_0 = 0)

let ziggurat_smoke_test _ =
  Random.init 10;
  let params =
    { zstd_dev = 122.0
    ; ztail_cut = 13.0
    ; zrectangle_count = 100
    } in
  let zigg = generate_ziggurat params in
  match zigg with
  | Some zigg ->
     validate_ziggurat zigg;
     let sample _ = sample_from_ziggurat zigg in
     let bounded x = abs x <= 3500 in
     let points = Array.init 1000 sample in
     assert_bool "not all points are bounded" (Array.for_all bounded points)
  | None ->
     assert_bool "generate_ziggurat failed" false

let suite : test =
  "TestRandomPolynomials" >:::
    ["gaussian_pdf_roundtrip" >:: gaussian_pdf_roundtrip;
      "ziggurat_smoke_test" >:: ziggurat_smoke_test]
