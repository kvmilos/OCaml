open Bstree

let sort l = 
  to_list (from_list l)

(* let%test "sortIntF" =
  List.equal Int.equal (sort [ 3; 2; 1 ]) [ 2; 1; 3]

let%test "sortIntT" =
  List.equal Int.equal (sort [ 3; 2; 1 ]) [ 1; 2; 3]

let%test "sortCharF" =
  List.equal Char.equal (sort [ 'c'; 'a'; 'b' ]) [ 'a'; 'c'; 'b' ]

let%test "sortCharT" =
  List.equal Char.equal (sort [ 'c'; 'a'; 'b' ]) [ 'a'; 'b'; 'c' ]

let%test "sortStringF" =
  List.equal String.equal (sort [ "kota"; "ma"; "ala" ]) [ "ala"; "ma"; "kota" ]

let%test "sortStringT" =
  List.equal String.equal (sort [ "kota"; "ma"; "ala" ]) [ "ala"; "kota"; "ma" ] *)