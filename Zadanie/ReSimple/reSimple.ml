(* DO DOPRACOWANIA *)

open SimpleRegexp

type t = char reg

let re (s : string) : t =
  parse s

let debug (r : t) =
  print_endline (to_string r)

let matches (r : t) (str : string) : bool =
  accepts r str
