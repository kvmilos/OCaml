open Lib.Check

let () =
  let n = int_of_string Sys.argv.(1) in
  if n < 0 then
    Printf.printf "Liczba ujemna\n"
  else if czy_pierwsza2 n then
    Printf.printf "Liczba pierwsza\n"
  else
    Printf.printf "Liczba złożona\n"
