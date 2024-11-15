open Commons

let poczatkowe_pierwsze = [2; 3; 5; 7; 11; 13; 17; 19]

let generuj_pierwsze limit =
  let rec pom n acc = 
    if n > limit then List.rev acc
    else if czy_pierwsza poczatkowe_pierwsze n then pom (n+1) (n::acc)
    else pom (n+1) acc
  in
  pom 2 []

let () =
  let limit = int_of_string Sys.argv.(1) in
  let pierwsze = generuj_pierwsze limit in
  let kod_l = String.concat "; " (List.map string_of_int pierwsze) in

  Printf.fprintf stdout "let l = [%s]" kod_l;