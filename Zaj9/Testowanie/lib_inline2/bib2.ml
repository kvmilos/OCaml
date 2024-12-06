open! Base

open Lib_inline1

let%test_unit "odw2" =
  [%test_eq: int list] (Bib1.odw [ 3; 2; 1 ]) [ 1; 2; ]
(* lepsze, bo w razie porażki wypisuje co z czym miało być takie same *)

(* potrzebuje open! Base; i ppx_assert *)

