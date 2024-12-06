open! Base
open Stdio
open Lib_inline1

let conc l = String.concat ~sep:" " (Bib1.odw l)

let%expect_test "trivial" = print_endline (conc ["World!"; "Hello" ])























































(*
let%expect_test "non-trivial" = 
  let hw = conc ["World!"; "Hello" ] in 
  print_endline hw;
  [%expect];
  let hwa = hw ^ " of Tanks" in
  print_endline hwa;
  [%expect];
  let phwa = conc [hwa; "Pretty"] in
  print_endline phwa;
  [%expect]
*)
