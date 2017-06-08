open OUnit2
open OUnitTest

let quick_test name fn =
  let len = Some Immediate in
  name >: test_case ?length:len fn

let show_list printer l =
  let shown_elements = List.map printer l in
  let interior = String.concat "; " shown_elements in
  Printf.sprintf "[%s]" interior

let show_array printer arr =
  let shown_elements = Array.map printer arr in
  let interior = String.concat "; " (Array.to_list shown_elements) in
  Printf.sprintf "[|%s|]" interior
