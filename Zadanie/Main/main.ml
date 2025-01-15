(* DO DOPRACOWANIA *)

open TestRe.TestRegexp

let () =
  let module TSimple = Test(ReSimple) in
  let (errors_simple, time_simple) = TSimple.test () in
  Printf.printf "ReSimple: %d errors in %f seconds\n" errors_simple time_simple;

  let module TReRe = Test(ReRe) in
  let (errors_rere, time_rere) = TReRe.test () in
  Printf.printf "ReRe: %d errors in %f seconds\n" errors_rere time_rere
