open Base
open Stdio
open Lib.Sort

let%expect_test "IntSortT" =
  let result = sort [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5] in
  List.iter result ~f:(fun n -> printf "%d " n);
  [%expect {| 1 1 2 3 3 4 5 5 5 6 9 |}]

let%expect_test "IntSortF" =
  let result = sort [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5] in
  List.iter result ~f:(fun n -> printf "%d " n);
  [%expect {| 1 1 2 3 4 3 5 5 5 6 9 |}]

let%expect_test "StringSortT" =
  let result = sort ["ala"; "ma"; "grubego"; "kota"] in
  List.iter result ~f:(fun s -> printf "%s " s);
  [%expect {| ala grubego kota ma |}]

let%expect_test "StringSortF" =
  let result = sort ["ala"; "ma"; "grubego"; "kota"] in
  List.iter result ~f:(fun s -> printf "%s " s);
  [%expect {| ala ma grubego kota |}]

let%expect_test "FloatSortT" =
  let result = sort [3.14; 2.71; 1.61; 1.41; 1.73] in
  List.iter result ~f:(fun f -> printf "%.2f " f);
  [%expect {| 1.41 1.61 1.73 2.71 3.14 |}]

let%expect_test "FloatSortF" =
  let result = sort [3.14; 2.71; 1.61; 1.41; 1.73] in
  List.iter result ~f:(fun f -> printf "%.2f " f);
  [%expect {| 1.61 1.41 1.73 2.71 3.14 |}]