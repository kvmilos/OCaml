(* DO DODANIA + DOPRACOWANIA *)

open Regexp

module Test (Re : REGEXP) = struct

  module StringSet = Set.Make(String)
  type lang = StringSet.t

  let make_lang (letters : string) (n : int) : lang =
    Random.self_init ();
    let max_len = 300 in

    let random_word () : string =
      let len = Random.int (max_len + 1) in
      String.init len (fun _ ->
        letters.[Random.int (String.length letters)]
      )
    in

    let rec loop i acc =
      if i <= 0 then acc
      else
        let w = random_word () in
        loop (i - 1) (StringSet.add w acc)
    in
    loop n StringSet.empty

  let select_accepted (re : Re.t) (words : lang) : lang =
    StringSet.filter (fun w -> Re.matches re w) words

  let test_two (r1 : Re.t) (r2 : Re.t) (words : lang) (debug : bool)
      : int * float =
    let t0 = Sys.time () in
    let acc1 = select_accepted r1 words in
    let acc2 = select_accepted r2 words in
    let time_used = Sys.time () -. t0 in

    let errors = 
      if StringSet.equal acc1 acc2 then 0
      else (
        if debug then Printf.eprintf "[test_two] Sets differ!\n";
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
        incr total_errors
      )
    in

    (* ========== 1. Złośliwe testy ========== *)
    let r_ba_star_b = Re.re "ba*b" in
    check r_ba_star_b "baabaabaaab" true;
    check r_ba_star_b "baaaaaaab" true;
    check r_ba_star_b "bab" true;
    check r_ba_star_b "bb" false;
    check r_ba_star_b "baab" true;

    let r_a_star_b = Re.re "a*b" in
    check r_a_star_b "aaaaab" true;
    check r_a_star_b "ab" true;
    check r_a_star_b "b" true;
    check r_a_star_b "a" false;

    (* ========== 2. Porównanie r*r i rr* ========== *)
    let r_a_star = Re.re "a*" in
    let r_aa_star = Re.re "aa*" in

    let lang_ab = make_lang "ab" 50 in
    let (err_ab, time_ab) = test_two r_a_star r_aa_star lang_ab false in
    total_errors := !total_errors + err_ab;
    Printf.printf "Test (a* vs aa*): errors=%d, time=%.3fs\n" err_ab time_ab;

    let r_ab_star_ab = Re.re "(a|b)*(a|b)" in
    let r_ab_ab_star = Re.re "(a|b)(a|b)*" in
    let lang_abc = make_lang "abc" 50 in
    let (err_abc, time_abc) = test_two r_ab_star_ab r_ab_ab_star lang_abc false in
    total_errors := !total_errors + err_abc;
    Printf.printf "Test ((a|b)*(a|b) vs (a|b)(a|b)*): errors=%d, time=%.3fs\n"
      err_abc time_abc;

    (* ========== 3. Porównanie r1|r2 i r2|r1 ========== *)
    let r_1 = Re.re "a(a|b)*|(a|b)*b" in
    let r_2 = Re.re "(a|b)*b|a(a|b)*" in
    let lang_ab2 = make_lang "ab" 50 in
    let (err_ab2, time_ab2) = test_two r_1 r_2 lang_ab2 false in
    total_errors := !total_errors + err_ab2;
    Printf.printf
      "Test (a(a|b)*|(a|b)*b vs (a|b)*b|a(a|b)*): errors=%d, time=%.3fs\n"
      err_ab2 time_ab2;

    (* 2 nietrywialne testy - samemu wymyślić, DO DODANIA *)

    let total_time = Sys.time () -. start_time in
    (!total_errors, total_time)
end
