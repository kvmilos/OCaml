open TestRe.TestRegexp

module type REGEXP_TEST = sig
  val name: string
  val test: unit -> int * float
end

let run_test (module M: REGEXP_TEST) =
  let errors, time = M.test () in
  Printf.printf "%s: %d errors in %f seconds\n" M.name errors time

module TSimple = struct
  let name = "ReSimple"
  include Test(ReSimple)
end

module TReRe = struct
  let name = "ReRe"
  include Test(ReRe)
end

let tests : (module REGEXP_TEST) list = [
  (module TSimple);
  (module TReRe);
]

let () =
  List.iter run_test tests
