
let odw l = List.rev l



(* małe testy przy definicji *)

let%test "odw" =
  List.equal Int.equal (odw [ 3; 2; 1 ]) [ 1; 2; 3]