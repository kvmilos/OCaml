open Regexp

module Test (Re : REGEXP) = struct

  module StringSet = Set.Make(String)
  type lang = StringSet.t

  let make_lang (alphabet : string) (n : int) : lang =
    Random.self_init ();
    let max_len = 500 in (* "500" można zamienić na "max n 500", ale przy 500 się łatwiej testuje :)  *)
    let random_word () : string =
      let len = Random.int (max_len + 1) in
      String.init len (fun _ ->
        alphabet.[Random.int (String.length alphabet)]
      )
    in
    let rec loop i acc =
      if i <= 0 then acc
      else
        let w = random_word () in
        if StringSet.mem w acc then loop i acc
        else loop (i - 1) (StringSet.add w acc)
    in
    loop n StringSet.empty

  let select_accepted (re : Re.t) (language : lang) : lang =
    StringSet.filter (fun w -> Re.matches re w) language

  let test_two (r1 : Re.t) (r2 : Re.t) (language : lang) (debug : bool)
      : int * float =
    let t0 = Sys.time () in
    let acc1 = select_accepted r1 language in
    let acc2 = select_accepted r2 language in
    let time_used = Sys.time () -. t0 in

    let errors = 
      if StringSet.equal acc1 acc2 then (
        if debug then Printf.printf "[test_two] Sets are equal!\n";(*Printf.printf "Regex1: "; Re.debug r1;Printf.printf "Regex2: "; Re.debug r2;*)
        0
      ) else (
        if debug then (
          Printf.printf "[test_two] Sets differ!\n";
          Printf.printf "Regex1: "; Re.debug r1;
          Printf.printf "Regex2: "; Re.debug r2;
          Printf.printf "Set1 (%d elements): %s\n" 
            (StringSet.cardinal acc1) 
            (String.concat ", " (StringSet.elements acc1));
          Printf.printf "Set2 (%d elements): %s\n" 
            (StringSet.cardinal acc2) 
            (String.concat ", " (StringSet.elements acc2))
        );
        1
      )
    in
    (errors, time_used)

  let test () : int * float =
    let start_time = Sys.time () in
    let total_errors = ref 0 in

    let check (re : Re.t) (s : string) (expected : bool) : unit =
      let got = Re.matches re s in
      if got <> expected then (
      Printf.printf "Error: while checking the word %s, got %b, expected %b on the regex" s got expected;
      Re.debug re;
      total_errors := !total_errors + 1
      ) else (
      if String.length(s) <= 50 then
        Printf.printf "OK: while checking the word %s, got %b, expected %b on the regex " s got expected
      else
        Printf.printf "OK: bardzo długie słowo, got %b, expected %b on the regex " got expected;
      Re.debug re
      )
    in

    (* 1. Złośliwe testy *)
    let r_ba_star_b = Re.re "ba*b" in
    check r_ba_star_b "baabaabaaab" false;
    check r_ba_star_b "baaaaaaab" true;
    check r_ba_star_b "bab" true;
    check r_ba_star_b "bb" true;
    check r_ba_star_b "baab" true;
    let long_string1 = "b" ^ String.make 9998 'a' ^ "b" in
    check r_ba_star_b long_string1 true;
    let long_string2 = "b" ^ String.make 9997 'a' ^ "b" ^ "b" in
    check r_ba_star_b long_string2 false;

    let r_a_star_b = Re.re "a*b" in
    check r_a_star_b "aaaaab" true;
    check r_a_star_b "ab" true;
    check r_a_star_b "b" true;
    check r_a_star_b "a" false;
    check r_a_star_b long_string1 false;
    check r_a_star_b long_string2 false;

    (* 2. Porównanie r*r i rr* *)
    let r_a_star_a = Re.re "a*a" in
    let r_aa_star = Re.re "aa*" in

    let lang_ab = make_lang "ab" 5000 in
    let (err_ab, time_ab) = test_two r_a_star_a r_aa_star lang_ab true in
    total_errors := !total_errors + err_ab;
    Printf.printf "Test (a* vs aa*): errors=%d, time=%.5fs\n" err_ab time_ab;

    let r_ab_star_ab = Re.re "(a|b)*(a|b)" in
    let r_ab_ab_star = Re.re "(a|b)(a|b)*" in
    let lang_abc = make_lang "abc" 5000 in
    let (err_abc, time_abc) = test_two r_ab_star_ab r_ab_ab_star lang_abc true in
    total_errors := !total_errors + err_abc;
    Printf.printf "Test ((a|b)*(a|b) vs (a|b)(a|b)*): errors=%d, time=%.5fs\n"
      err_abc time_abc;

    (* 3. Porównanie r1|r2 i r2|r1 *)
    let r_1 = Re.re "a(a|b)*|(a|b)*b" in
    let r_2 = Re.re "(a|b)*b|a(a|b)*" in
    let lang_ab2 = make_lang "ab" 5000 in
    let (err_ab2, time_ab2) = test_two r_1 r_2 lang_ab2 true in
    total_errors := !total_errors + err_ab2;
    Printf.printf
      "Test (a(a|b)*|(a|b)*b vs (a|b)*b|a(a|b)*): errors=%d, time=%.5fs\n"
      err_ab2 time_ab2;

    (* 4. moje testy *)
    let r_3 = Re.re "a*b|b" in
    let r_4 = Re.re "a*b" in
    let lang_ab3 = make_lang "abc" 5000 in
    let (err_ab3, time_ab3) = test_two r_3 r_4 lang_ab3 true in
    total_errors := !total_errors + err_ab3;
    Printf.printf "Test (a*b|b vs a*b): errors=%d, time=%.5fs\n" err_ab3 time_ab3;

    (* let r_5 = Re.re "aa*bc" in
    let r_6 = Re.re "a*abc|abc" in
    let lang_ab4 = make_lang "abc" 5000 in
    let (err_ab4, time_ab4) = test_two r_5 r_6 lang_ab4 true in
    total_errors := !total_errors + err_ab4;
    Printf.printf "Test (aa*bc vs a*abc|bc): errors=%d, time=%.5fs\n" err_ab4 time_ab4; *)
    (* TO BYŁ JEDNAK ZŁY TEST, aa*bc to coś innego niż a*abc|bc (to drugie == a*bc) *)

    let r_7 = Re.re "(a*)*" in
    let r_8 = Re.re "a*" in
    let lang_ab5 = make_lang "ab" 5000 in
    let (err_ab5, time_ab5) = test_two r_7 r_8 lang_ab5 true in
    total_errors := !total_errors + err_ab5;
    Printf.printf "Test ((a*)* vs a*): errors=%d, time=%.5fs\n" err_ab5 time_ab5;

    let r_9 = Re.re "(a|b)*|a" in
    let r_10 = Re.re "(a|b)|((a|b)|(a|b))*" in
    let lang_ab6 = make_lang "ab" 5000 in
    let (err_ab6, time_ab6) = test_two r_9 r_10 lang_ab6 true in
    total_errors := !total_errors + err_ab6;
    Printf.printf "Test ((a|b)*|a vs (a|b)|a*): errors=%d, time=%.5fs\n" err_ab6 time_ab6;

    let r_11 = Re.re "abc|a*bc" in
    let r_12 = Re.re "a*bc" in
    let lang_ab7 = make_lang "abc" 5000 in
    let (err_ab7, time_ab7) = test_two r_11 r_12 lang_ab7 true in
    total_errors := !total_errors + err_ab7;
    Printf.printf "Test (abc|a*bc vs a*bc): errors=%d, time=%.5fs\n" err_ab7 time_ab7;

    Printf.printf "NOWE: \n";
    let r_13 = Re.re "(ab)(cd)" in
    let r_14 = Re.re "a(b(cd))" in
    let lang_ab8 = make_lang "abcd" 5000 in
    let (err_ab8, time_ab8) = test_two r_13 r_14 lang_ab8 true in
    total_errors := !total_errors + err_ab8;
    Printf.printf "Test ((ab)(cd) vs a(b(cd))): errors=%d, time=%.5fs\n" err_ab8 time_ab8;

    let r_15 = Re.re "a(b|c)d" in
    let r_16 = Re.re "a(bd|cd)" in
    let lang_ab9 = make_lang "abcd" 5000 in
    let (err_ab9, time_ab9) = test_two r_15 r_16 lang_ab9 true in
    total_errors := !total_errors + err_ab9;
    Printf.printf "Test (a(b|c)d vs a(bd|cd)): errors=%d, time=%.5fs\n" err_ab9 time_ab9;

    let r_17 = Re.re "(((a*)*)*)*" in
    let r_18 = Re.re "a*" in
    let lang_ab10 = make_lang "ab" 5000 in
    let (err_ab10, time_ab10) = test_two r_17 r_18 lang_ab10 true in
    total_errors := !total_errors + err_ab10;
    Printf.printf "Test ((((a*)*)*)* vs a*): errors=%d, time=%.5fs\n" err_ab10 time_ab10;

    let r_19 = Re.re "a(b((cd)ef)g)" in
    let r_20 = Re.re "a(b(c(d(ef)(fg))))" in
    let lang_ab11 = make_lang "abcdefg" 5000 in
    let (err_ab11, time_ab11) = test_two r_19 r_20 lang_ab11 true in
    total_errors := !total_errors + err_ab11;
    Printf.printf "Test (a(b((cd)ef)g) vs a(b(c(d(ef)(fg))))): errors=%d, time=%.5fs\n" err_ab11 time_ab11;

    let extra_patterns = [
      ("(a|a)", "a", true);
      ("(a|a)", "b", false);
      ("(a*)*", "", true);
      ("(a*)*", "aaa", true);
      ("(a|b)*|a", "aaaa", true);
      ("(a|b)*|a", "ababab", true);
      ("((a|b)|(a|b))*", "ababab", true);
      ("((a|b)|(a|b))*", "abc", false);
      ("a*a", "aaa", true);
      ("aa*", "aaa", true);
      ("a*a", "b", false);
      (* NOWE *)
      ("(a(b(c(d(ef*)))))", "abcde", true);
      ("(a(b(c(d(ef*)))))", "abcdefffffff", true);
    ] in
    List.iter (fun (pattern, word, expected) ->
      let r = Re.re pattern in
      check r word expected
    ) extra_patterns;

    let total_time = Sys.time () -. start_time in
    (!total_errors, total_time);
    
end
