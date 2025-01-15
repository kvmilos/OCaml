(* DO DOPRACOWANIA *)

[@@@warning "-33"] (* wyłączenie ostrzeżenia o nieużywaniu TestRe *)
open TestRe
module RP = Re.Pcre

type t = {
  original : string;
  compiled : RP.regexp;
}

let re (pattern : string) : t =
  let anchored = Printf.sprintf "^(%s)$" pattern in
  let compiled_pat = RP.regexp anchored in
  { original = pattern; compiled = compiled_pat }

let debug (r : t) : unit =
  print_endline r.original

let matches (r : t) (s : string) : bool =
  RP.pmatch ~rex:r.compiled s
