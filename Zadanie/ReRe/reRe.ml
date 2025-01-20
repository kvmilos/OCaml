open Re.Pcre

type t = {
  original : string;
  compiled : regexp;
}

let re (pattern : string) : t =
  let anchored = Printf.sprintf "^(%s)$" pattern in
  let compiled_pat = regexp anchored in
  { original = pattern; compiled = compiled_pat }

let debug (r : t) : unit =
  print_endline r.original

let matches (r : t) (s : string) : bool =
  pmatch ~rex:r.compiled s

(* Sekcja testowa *)
let () =
(* Utworzenie wyrażenia regularnego *)
let r = re "a*b" in
(* Wypisanie oryginalnego wzorca *)
debug r;

(* Testy dopasowania *)
let test_strings = ["b"; "ab"; "aaab"; "aabx"; "aba"] in
List.iter (fun s ->
  Printf.printf "matches \"%s\" against pattern \"%s\": %b\n" s r.original (matches r s)
) test_strings

