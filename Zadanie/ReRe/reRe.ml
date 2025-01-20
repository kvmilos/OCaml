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
  print_endline (r.original)

let matches (r : t) (s : string) : bool =
  pmatch ~rex:r.compiled s

(* let () =
  let tests = [
    ("a*b", [("b", true); ("ab", true); ("aaab", true); ("a", false); ("aba", false)]);
    ("(foo|bar)(foo|bar)*", [
      ("foo", true); 
      ("bar", true); 
      ("foobar", true); 
      ("barfoo", true); 
      ("", false)
    ]);
    ("(a|b)*c", [("aaac", true); ("bbc", true); ("c", true); ("ab", false)]);
    ("(hello)(hello)*", [("hello", true); ("hellohello", true); ("hell", false)]);
    ("", [("", true); ("a", false)]);  (* Pusty wzorzec *)
    ("(a|b|c|d|e|f|g)*", [("abc", true); ("", true); ("h", false)]);
  ] in

  List.iter (fun (pattern, cases) ->
    let r = re pattern in
    debug r;
    List.iter (fun (input, expected) ->
      let result = matches r input in
      Printf.printf "Pattern \"%s\" on input \"%s\": expected %b, got %b\n"
        pattern input expected result
    ) cases;
    print_endline "---------------------"
  ) tests *)