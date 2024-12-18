
let b_expr = `Add (`Var "x", `Int 3)

let e_expr = `If (`Bool false, 
                  b_expr, 
                  `Add (`If (`Bool true, `Int 100, `Var "y"), `Int 10))

open! Basic
open! Ext                  

let _ = 

  print_endline "CHECK BASIC EXPRESSION.";
  print_endline "--> WITH BASIC STUFF";
  check_basic b_expr;
  print_endline "--> WITH EXT STUFF";
  check_ext b_expr;

  print_endline "";
  print_endline "CHECK EXTENDED EXPRESSION.";
  print_endline "--> WITH BASIC STUFF";
  print_endline "<impossible> "; (* check_basic e_expr; *)
  print_endline "--> WITH EXT STUFF";
  check_ext e_expr;

  print_endline "\nTHIS IS THE END..."

