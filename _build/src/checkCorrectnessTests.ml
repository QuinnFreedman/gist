open Batteries;;
open OUnit2;;
open Types;;
open Parser;;
open CheckCorrectness;;


let correctness_checker_suite = "check correctness test suite" >:::
  [
    "always pass">::
    (fun _ ->
       assert_equal
         ~printer:dump
         1 1);

    "check var exists">::
    (fun _ ->
       assert_equal
         ~printer:dump
         ["Error @ (1:1): variable `x` is not defined here"]
         (check_correctness (parse_string "x")));

    "check var exists true">::
    (fun _ ->
       assert_equal
         ~printer:dump
         []
         (check_correctness (parse_string "(def x 3 x)")));


  ];;
