open Bstree
open Sort
open Expr

let () =
  let ints = [5; 3; 8; 1; 2] in
  Printf.printf "Sorted ints: %s\n" (String.concat " " (List.map string_of_int (sort_int ints)));

  let strings = ["banana"; "apple"; "cherry"; "date"] in
  Printf.printf "Sorted strings: %s\n" (String.concat " " (sort_string strings));

  let exprs = [
    parse "23+";
    parse "34*";
    parse "5-";
    parse "12+3*"
  ] in

  let sorted_exprs = sort_expr exprs in
  Printf.printf "Sorted expressions by value:\n";
  List.iter (fun e -> Printf.printf "%d " (calcf (fun _ -> 0) e)) sorted_exprs

