(*
                         CS 51 Problem Set 2
            Higher Order Functional Programming -- Testing
 *)

open Bignum;;

open Test_simple ;;      (* a really simple unit testing framework *)

let test () =
  unit_test ((negate {neg = true; coeffs = [0; 0; 0]}) = {neg = true; coeffs = [0; 0; 0]})
    "negate zeroes";



  unit_test ((equal {neg = true; coeffs = [0; 0; 0]}{neg = true; coeffs = [0; 0; 0]}) = true)
    "compare 0s";
  unit_test ((equal {neg = true; coeffs = [0; 0]}{neg = true; coeffs = [0; 0; 0]}) = true)
    "compare different 0s";



  unit_test ((less {neg = true; coeffs = [0; 0]}{neg = true; coeffs = [0; 0]}) = false)
    "less same";



  unit_test ((greater {neg = true; coeffs = [0; 0]}{neg = true; coeffs = [0; 0]}) = false)
    "less same";



  unit_test ((to_int {neg = true; coeffs = [80; 45; 029]}) = Some 0)
    "to_int empty";




  unit_test ((from_int 6000) = {neg = false; coeffs = [6; 0]})
    "from_int 6000";



  unit_test ((plus {neg = false; coeffs = [6;0]}{neg = false; coeffs = [6;0]}) = {neg = true; coeffs = [12;0]})
    "plus 12000";



  unit_test ((times {neg = false; coeffs = [6;0]}{neg = false; coeffs = [6;0]}) = {neg = true; coeffs = [12;0]})
    "plus 12000";



  () ;;

test ();;
