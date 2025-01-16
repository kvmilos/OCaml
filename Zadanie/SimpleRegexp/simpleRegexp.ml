open SimpleRegexpDef

type 'c reg = 'c SimpleRegexpDef.reg

  let rec simpl = function
  | Or(r, Empty) | Or(Empty, r) -> simpl r (* a|∅ = ∅|a = a *)
  | Or(r, Eps) | Or(Eps, r) -> simpl r
  | Concat(Eps, r) | Concat(r, Eps) -> simpl r (* εa = aε = a *)
  | Or(r1, r2) ->
    let sr1 = simpl r1 in
    let sr2 = simpl r2 in
    if sr1 = sr2 then sr1 (* a|a = a *)
    else if sr1 = Star(sr2) || sr2 = Star(sr1) then simpl (Star sr1) (* a|a* = a*|a = a* *)
    else if match sr1, sr2 with
      | Concat(Star r, Concat(r', r'')), r2 when r = r2 && r = r' && r = r'' -> true
      | r2, Concat(Star r, Concat(r', r'')) when r = r2 && r = r' && r = r'' -> true
      | _ -> false then
        match sr1, sr2 with
        | Concat(Star r, Concat(r', r'')), r2 when r = r2 && r = r' && r = r'' -> simpl (Concat(r', Star r)) (* (a*aa)|a = aa* *)
        | r2, Concat(Star r, Concat(r', r'')) when r = r2 && r = r' && r = r'' -> simpl (Concat(r', Star r)) (* a|(a*aa) = aa* *)
        | _ -> simpl (Star (simpl sr1))
    else Or(sr1, sr2) (* a|b = a|b *)
  | Concat(r1, r2) -> Concat(simpl r1, simpl r2) (* a[b|b] = ab *)
  | Star r -> 
    let sr = simpl r in
    (match sr with
    | Star _ -> sr (* a** = a* *)
    | _ -> Star sr) (* a* = a* *)
  | r -> r (* a = a *)



let rec nullable = function
  | Eps -> true
  | Lit _ -> false
  | Empty -> false
  | Concat(r1, r2) -> nullable r1 && nullable r2
  | Or(r1, r2) -> nullable r1 || nullable r2
  | Star _ -> true
  
let rec empty = function
  | Empty -> true
  | Eps -> false
  | Lit _ -> false
  | Concat(r1, r2) -> empty r1 || empty r2
  | Or(r1, r2) -> empty r1 && empty r2
  | Star _ -> false

let rec der c = function (* w komentarzach robię pochodną po 'a' *) 
  | Empty -> Empty (* ∅ -> ∅ *)
  | Eps -> Empty (* ε -> ∅ *)
  | Lit x -> if x = c then Eps else Empty (* a -> ε, b -> ∅ *)
  | Or(r1, r2) -> simpl (Or(simpl (der c r1), simpl (der c r2))) (* a*c|ab -> a*c|b *)
  | Concat(r1, r2) -> 
    let dr1 = simpl (der c r1) in
    let dr2 = simpl (der c r2) in
    if nullable r1 then simpl (Or(simpl (Concat( dr1, r2)), dr2))
    else simpl (Concat(dr1, r2))
  | Star r -> simpl (Concat(der c r, Star r))

let ders cs r =
  List.fold_left (fun acc c -> der c acc) r cs

let accept r cs =
    let r' = ders cs r in
    nullable r'
  
let rec repr f = function
  | Empty -> "∅"
  | Eps -> "ε"
  | Lit c -> f c
  | Or(r1, r2) ->
    let r1_str = repr f r1 in
    let r2_str = repr f r2 in
    let r1_wrapped = match r1 with Or _ -> r1_str | _ -> "(" ^ r1_str ^ ")" in
    let r2_wrapped = match r2 with Or _ -> r2_str | _ -> "(" ^ r2_str ^ ")" in
    r1_wrapped ^ "|" ^ r2_wrapped
  | Concat(r1, r2) ->
    let r1_str = repr f r1 in
    let r2_str = repr f r2 in
    let r1_wrapped = match r1 with Or _ -> "(" ^ r1_str ^ ")" | _ -> r1_str in
    let r2_wrapped = match r2 with Or _ -> "(" ^ r2_str ^ ")" | _ -> r2_str in
    r1_wrapped ^ r2_wrapped
  | Star r ->
    let r_str = repr f r in
    if match r with Or _ -> true | _ -> false then "(" ^ r_str ^ ")*"
    else r_str ^ "*"
  
let char c = Lit c

let rec string s =
  if s = "" then Eps
  else Concat (Lit s.[0], string (String.sub s 1 (String.length s - 1)))

let rec alts s =
  match s with
  | "" -> Empty
  | _ ->
    let c = s.[0] in
    let rest = String.sub s 1 (String.length s - 1) in
    Or (Lit c, alts rest)  

let accepts r s =
  accept r (List.init (String.length s) (String.get s))

let to_string (r : char reg) : string =
  repr (fun c -> String.make 1 c) r

let parse s =
  Parser.regex Lexer.token (Lexing.from_string s)

let () =
  let r1 = Concat(Star(Lit 'a'), Lit 'a') in
  let r2 = Star(Or(Lit 'a', Lit 'b')) in
  let r3 = Concat(Star(Lit 'a'), Concat(Lit 'a', Lit 'a')) in
  let r4 = Concat(Concat(Lit 'a', Star(Lit 'a')), Concat(Lit 'b', Lit 'c')) in
  let r5 = Concat(Star(Lit 'a'), Concat(Lit 'a', Concat(Lit 'b', Lit 'c'))) in

  Printf.printf "Testowanie funkcji der (oraz simpl): \n";
  Printf.printf "der 'a' (a*a): %s\n" (to_string (der 'a' r1));
  Printf.printf "der 'a' ((a|b)*): %s\n" (to_string (der 'a' r2));
  Printf.printf "der 'a' (a*aa): %s\n" (to_string (der 'a' r3));
  Printf.printf "der 'a' (aa*bc): %s\n" (to_string (der 'a' r4));
  Printf.printf "der 'a' (a*abc): %s\n" (to_string (der 'a' r5));

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Lit 'a')), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*aa)|(a): %s\n" (to_string simplified);   (* działa *)

  let r = Or(Or(Lit 'a', Lit 'b'), Lit 'b') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)|b: %s\n" (to_string simplified);   (* nie działa *)

  let r = Or(Star(Lit 'a'), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*)|a: %s\n" (to_string simplified);   (* działa *)

  let r = Or(Concat(Star(Lit 'a'), Lit 'b'), Lit 'b') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*b)|b: %s\n" (to_string simplified);   (* nie działa *)
  
  let r = Or(Concat(Star(Lit 'a'), Lit 'b'), Concat(Lit 'a', Lit 'b')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*b)|(ab): %s\n" (to_string simplified);  (* nie działa *)

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Concat(Lit 'b', Lit 'c'))), Concat(Lit 'b', Lit 'c')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*abc)|(bc): %s\n" (to_string simplified);  (* nie działa *)

  let r = Star(Star(Lit 'a')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*)*: %s\n" (to_string simplified);  (* działa *)

  let r = Or(Star(Or(Lit 'a', Lit 'b')), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)*|a: %s\n" (to_string simplified);  (* nie działa *)

  let r = Or(Star(Or(Lit 'a', Lit 'b')), Or(Lit 'a', Lit 'b')) in
  let simplified = simpl r in
  Printf.printf "Simplifying ((a|b)*)|((a|b)): %s\n" (to_string simplified);   (* prawie działa - nawiasy *)