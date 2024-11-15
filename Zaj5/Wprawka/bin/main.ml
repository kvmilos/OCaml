open Wprawka

let arg_as_int i = int_of_string Sys.argv.(i)
let liczba = Biblioteka.oblicz (arg_as_int 1) (arg_as_int 2) 

let () = print_endline (string_of_int liczba)

(* Testowanie: 
   dune exec Wprawka -- 2 3 *)