
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

  print_endline "\nTHIS WAS THE END...";

  print_endline "\nADDITIONAL EXT TESTS:";
  let e1 = `Bool true in
  let e2 = `If (`Bool true, `Int 42, `Int 0) in
  let e3 = `If (`Bool false, `Var "z", `Add (`Var "z", `Int 1)) in
  let e4 = `Add (`Int 10, `Var "x") in
  let e5 = `Add (`If (`Bool true, `Int 2, `Int 3), `If (`Bool false, `Int 10, `Int 20)) in

  check_ext e1;
  check_ext e2;
  check_ext e3;
  check_ext e4;
  check_ext e5;