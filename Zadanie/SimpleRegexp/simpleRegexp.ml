open SimpleRegexpDef

type 'c reg = 'c SimpleRegexpDef.reg

(** Upraszczanie wyrażeń regularnych używające prostych własności, 
    oczywiście bez zmiany języka akceptowanych słów. *)
let rec simpl = function
| Eps -> Eps (* ε = ε *)
| Empty -> Empty (* ∅ = ∅ *)
| (Lit c) -> Lit c (* a = a *)
| (Or(r1, r2)) -> begin
  let sr1 = simpl r1 in
  let sr2 = simpl r2 in
  if sr1 = sr2 then sr1 (* a|a = a *)
  else if sr1 = Empty then sr2 (* ∅|a = a *)
  else if sr2 = Empty then sr1 (* a|∅ = a *)
  else match (sr1, sr2) with
    | (Or(r, _), r') when r = r' -> sr1 (* (a|b)|a = a|b *)
    | (Or(_, r), r') when r = r' -> sr1 (* (a|b)|b = a|b *)
    | (r, Or(_, r')) when r = r' -> sr2 (* a|(b|a) = b|a *)
    | (r, Or(r', _)) when r = r' -> sr2 (* a|(a|b) = b|a *)
    | (Eps, Concat(r, Star(r'))) when r = r' -> simpl (Star(r')) (* ε|aa* = a* *)
    | (Eps, Concat(Star(r'), r)) when r = r' -> simpl (Star(r')) (* ε|a*a = a* *)
    | (Concat(Star r', r), Eps) when r = r' -> simpl (Star(r')) (* a*a|ε = a* *)
    | (Concat(r, Star r'), Eps) when r = r' -> simpl (Star(r')) (* aa*|ε = a* *)
    | (Star(r), r') when r = r' -> sr1 (* a*|a = a* *)
    | (r', Star(r)) when r = r' -> sr2 (* a|a* = a* *)
    | (Concat(Star r, Concat(r', r'')), r''') when r = r''' && r = r' && r = r'' -> simpl (Concat(r', Star r)) (* (a*aa)|a = aa* *)
    | (r''', Concat(Star r, Concat(r', r''))) when r = r''' && r = r' && r = r'' -> simpl (Concat(r', Star r)) (* a|(a*aa) = aa* *)
    | (Concat(Star s1, Concat(s2, s3)), Concat(s4, s5)) when s2 = s4 && s2 = s1 && s3 = s5 -> simpl (Concat(Star s1, s3)) (* (a*b)c|ab = a*bc *)
    | (Concat(s4, s5), Concat(Star s1, Concat(s2, s3))) when s2 = s4 && s2 = s1 && s3 = s5 -> simpl (Concat(Star s1, s3)) (* ab|(a*b)c = a*bc *)
    | (Star(Or(r1, r2)), r') when r' = r1 || r' = r2 -> Star(Or(r1,r2)) (* (a|b)*|a = (a|b)* *)
    | (r', Star(Or(r1, r2))) when r' = r1 || r' = r2 -> Star(Or(r1,r2)) (* a|(a|b)* = (a|b)* *)
    | (Concat(Star r, r'), r'') when r' = r'' -> simpl (Concat(Star r, r')) (* a*b|b = a*b *)
    | (r'', Concat(Star r, r')) when r' = r'' -> simpl (Concat(Star r, r')) (* b|a*b = a*b *)
    | (Concat(Star s1, s2), Concat(s3, s4)) when s1 = s3 && s2 = s4 -> simpl (Concat(Star s1, s2)) (* a*b|ab = a*b *)
    | (Concat(s3, s4), Concat(Star s1, s2)) when s1 = s3 && s2 = s4 -> simpl (Concat(Star s1, s2)) (* ab|a*b = a*b *)
    | (Concat(Star s1, Concat(s2, s3)), s4) when s1 = s2 && s3 = s4 -> simpl (Concat(Star s1, s3)) (* (a*abc|bc = a*bc *)
    | (s4, Concat(Star s1, Concat(s2, s3))) when s1 = s2 && s3 = s4 -> simpl (Concat(Star s1, s3)) (* abc|a*bc = a*bc *)
    | _ -> Or(sr1, sr2) (* a|b = a|b *) 
  end
| (Concat(r1, r2)) -> begin
  let sr1 = simpl r1 in
  let sr2 = simpl r2 in
  match (sr1, sr2) with
  | Concat(s1, s2), s3 -> Concat(s1, Concat(s2, s3)) (* NOWE: (ab)c -> a(bc) *)
  | (Eps, _) -> simpl r2 (* εa = a *)
  | (_, Eps) -> simpl r1 (* aε = a *)
  | (_, _) -> Concat(simpl r1, simpl r2) (* (a|a)a = aa *) 
  end
| Star r1 -> let sr = simpl r1 in
    (match sr with
    | Star _ -> sr (* a** = a* *)
    | _ -> Star sr) (* a* = a* *)
    
(** Czy ε należy do języka? *)
    let rec nullable = function
  | Eps -> true
  | Lit _ -> false
  | Empty -> false
  | Concat(r1, r2) -> nullable r1 && nullable r2
  | Or(r1, r2) -> nullable r1 || nullable r2
  | Star _ -> true
  
(** Czy akceptowany język jest pusty? *)
let rec empty = function
  | Empty -> true
  | Eps -> false
  | Lit _ -> false
  | Concat(r1, r2) -> empty r1 || empty r2
  | Or(r1, r2) -> empty r1 && empty r2
  | Star _ -> false

(** [der a r] to pochodna [r] względem [a], czyli wyrażenie regularne [r'], 
    które akceptuje takie słowa [w], że słowo [aw] jest akceptowane przez [r].
    Np. pochodna wyrażenia ab* po a to oczywiście b*, pochodna a* po a to a*, 
    a pochodna b po a to ∅ (wyrażenie nieakceptujące żadnego słowa). *)
  let rec der c = function (* w komentarzach robię pochodną po 'a' *) 
  | Empty -> Empty (* ∅ -> ∅ *)
  | Eps -> Empty (* ε -> ∅ *)
  | Lit x -> if x = c then Eps else Empty (* a -> ε, b -> ∅ *)
  | Or(r1, r2) -> simpl (Or(simpl (der c r1), simpl (der c r2))) (* a*c|ab -> a*c|b *)
  | Concat(r1, r2) ->
    let dr1 = simpl (der c r1) in
    let dr2 = simpl (der c r2) in
    if nullable r1 then simpl (Or(simpl (Concat(dr1, r2)), dr2))
    else simpl (Concat(dr1, r2))
  | Star r -> simpl (Concat(der c r, Star r))

(** [ders v r] to pochodna [r] względem [v], czyli wyrażenie regularne [r'], 
    które akceptuje takie słowa [w], że słowo [vw] jest akceptowane przez [r].
    W implementacji należy użyć [der], niewątpliwie przyda się też [simpl]. *)
let ders cs r =
  List.fold_left (fun acc c -> der c acc) r cs

(** Czy słowo jest akceptowane przez wyrażenie regularne?
    W implementacji należy użyć [ders] i [nullable]. *)
let accept r cs =
    let r' = ders cs r in
    nullable r'
  
(** Prezentacja wyrażenia regularnego w postaci napisu. *)    
let rec repr f = function
  | Empty -> "∅"
  | Eps -> "ε"
  | Lit c -> f c
  | Or(r1, r2) ->
    let r1_str = repr f r1 in
    let r2_str = repr f r2 in
    r1_str ^ "|" ^ r2_str
  | Concat(r1, r2) ->
    let r1_str = repr f r1 in
    let r2_str = repr f r2 in
    let r1_wrapped = match r1 with Or _ | Concat _ -> "(" ^ r1_str ^ ")" | _ -> r1_str in (* część "| Concat _" dodana tylko na potrzebę testu (ab)(cd) -> (a(b(cd))) *)
    let r2_wrapped = match r2 with Or _ | Concat _ -> "(" ^ r2_str ^ ")" | _ -> r2_str in (* część "| Concat _" dodana tylko na potrzebę testu (ab)(cd) -> (a(b(cd))) *)
    r1_wrapped ^ r2_wrapped
  | Star r ->
    let r_str = repr f r in
    if match r with Or _ -> true | Concat _ -> true | _ -> false then "(" ^ r_str ^ ")*"
    else r_str ^ "*"
  
(** Wyrażenie regularne akceptujące język złożony z jednego jednoliterowego słowa. *)
let char c = Lit c

(** Wyrażenie regularne akceptujące język złożony z jednego słowa. *)
let rec string s =
  if s = "" then Eps
  else Concat (Lit s.[0], string (String.sub s 1 (String.length s - 1)))

(** Wyrażenie regularne akceptujace język złożony z jednoliterowych słów 
    dla wszystkich liter z podanego napisu. *)
let rec alts s =
  match s with
  | "" -> Empty
  | _ ->
    let c = s.[0] in
    let rest = String.sub s 1 (String.length s - 1) in
    Or (Lit c, alts rest)  

(** Specjalizacja accept dla [char reg] i [string]. *)    
let accepts r s =
  accept r (List.init (String.length s) (String.get s))

(** Zamiana [char reg] na napis. Proszę postarać się o niewstawianie niepotrzebnych nawiasów. *)  
let to_string (r : char reg) : string =
  repr (fun c -> String.make 1 c) r

(** Zamiana napisu na wyrażenie regularne *)
let parse s =
  Parser.regex Lexer.token (Lexing.from_string s)

let () =
  let r1 = Concat(Star(Lit 'a'), Lit 'a') in
  let r2 = Star(Or(Lit 'a', Lit 'b')) in
  let r3 = Concat(Star(Lit 'a'), Concat(Lit 'a', Lit 'a')) in
  let r4 = Concat(Concat(Lit 'a', Star(Lit 'a')), Concat(Lit 'b', Lit 'c')) in
  let r5 = Concat(Star(Lit 'a'), Concat(Lit 'a', Concat(Lit 'b', Lit 'c'))) in

  Printf.printf "Testowanie funkcji der oraz simpl: \n";
  Printf.printf "der 'a' a*a: %s\n" (to_string (der 'a' r1));
  Printf.printf "der 'a' (a|b)*: %s\n" (to_string (der 'a' r2));
  Printf.printf "der 'a' a*aa: %s\n" (to_string (der 'a' r3));
  Printf.printf "der 'a' aa*bc: %s\n" (to_string (der 'a' r4));
  Printf.printf "der 'a' a*abc: %s\n" (to_string (der 'a' r5));

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Lit 'a')), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying a*aa|a: %s\n" (to_string simplified);

  let r = Or(Or(Lit 'a', Lit 'b'), Lit 'b') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)|b: %s\n" (to_string simplified);

  let r = Or(Star(Lit 'a'), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying a*|a: %s\n" (to_string simplified);

  let r = Or(Concat(Star(Lit 'a'), Lit 'b'), Lit 'b') in
  let simplified = simpl r in
  Printf.printf "Simplifying a*b|b: %s\n" (to_string simplified);
  
  let r = Or(Concat(Star(Lit 'a'), Lit 'b'), Concat(Lit 'a', Lit 'b')) in
  let simplified = simpl r in
  Printf.printf "Simplifying a*b|ab: %s\n" (to_string simplified);

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Concat(Lit 'b', Lit 'c'))), Concat(Lit 'b', Lit 'c')) in
  let simplified = simpl r in
  Printf.printf "Simplifying a*abc|bc: %s\n" (to_string simplified);

  let r = Star(Star(Lit 'a')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*)*: %s\n" (to_string simplified);

  let r = Or(Star(Or(Lit 'a', Lit 'b')), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)*|a: %s\n" (to_string simplified);

  let r = Or(Star(Or(Lit 'a', Lit 'b')), Or(Lit 'a', Lit 'b')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)*|(a|b): %s\n" (to_string simplified);

  Printf.printf "NOWE: \n";
  Printf.printf "Disclaimer: zmieniłem wypisywanie nawiasów, tylko po to, żeby było widać nawiasy przy konkatenacjach. Tylko na potrzeby tego testu.\n";

  let r = Concat(Concat(Lit 'a', Lit 'b'), Lit 'c') in
  let simplified = simpl r in
  Printf.printf "Simplifying (ab)c: %s\n" (to_string simplified);

  let r = Concat(Concat(Lit 'a', Lit 'b'), Concat(Lit 'c', Lit 'd')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (ab)(cd): %s\n" (to_string simplified);

  let r = Concat(Concat(Concat(Lit 'a', Lit 'b'), Lit 'c'), Concat(Lit 'd', Lit 'e')) in
  let simplified = simpl r in
  Printf.printf "Simplifying ((ab)c)(de): %s\n" (to_string simplified);

  let r = Concat(Concat(Concat(Concat(Lit 'a', Lit 'b'), Lit 'c'), Lit 'd'), Lit 'e') in
  let simplified = simpl r in
  Printf.printf "Simplifying (((ab)c)d)e: %s\n" (to_string simplified);

  let r = Concat(Star(Concat(Lit 'a', Lit 'b')), Lit 'c') in
  let simplified = simpl r in
  Printf.printf "Simplifying (ab)*c: %s\n" (to_string simplified);

  let r = Concat(Star(Star(Lit 'a')), Concat(Lit 'b', Lit 'c')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a**)(bc): %s\n" (to_string simplified);

  let r = Concat(Or(Lit 'a', Lit 'b'), Concat(Lit 'c', Or(Lit 'd', Lit 'e'))) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)(c(d|e)): %s\n" (to_string simplified);

  let r = Concat(Or(Concat(Lit 'a', Lit 'b'), Lit 'c'), Lit 'd') in
  let simplified = simpl r in
  Printf.printf "Simplifying ((ab)|c)d: %s\n" (to_string simplified);

  let r = Concat(
    Concat(Star(Lit 'a'), Concat(Lit 'b', Lit 'c')),
    Concat(Lit 'd', Concat(Star(Lit 'e'), Lit 'f'))
  ) in
  let simplified = simpl r in
  Printf.printf "Simplifying ((a*)(bc))((d)((e*)f)): %s\n" (to_string simplified);

  let r = Concat(
    Star(Concat(Lit 'a', Or(Lit 'b', Lit 'c'))),
    Concat(Concat(Lit 'd', Lit 'e'), Star(Lit 'f'))
  ) in
  let simplified = simpl r in
  Printf.printf "Simplifying ((a(b|c))*)(de)(f*): %s\n" (to_string simplified);

  let r = Concat(
    Concat(Star(Lit 'a'), Star(Concat(Lit 'b', Lit 'c'))),
    Concat(Or(Lit 'd', Lit 'e'), Or(Lit 'f', Lit 'g'))
  ) in
  let simplified = simpl r in
  Printf.printf "Simplifying ((a*)(bc)*)((d|e)(f|g)): %s\n" (to_string simplified);

  let r = Star(Star(Star(Star(Star(Star(Star(Star(Star(Star(Lit 'a')))))))))) in
  let simplified = simpl r in
  Printf.printf "Simplifying (((((((((a*)*)*)*)*)*)*)*)*)*: %s\n" (to_string simplified);

  (*   (* aa*bc vs a*abc|bc) *) *)
  let r = Concat(Lit 'a', Concat(Star(Lit 'a'), Concat(Lit 'b', Lit 'c'))) in
  let simplified = simpl r in
  Printf.printf "Simplifying aa*bc: %s\n" (to_string simplified);

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Concat(Lit 'b', Lit 'c'))), Concat(Lit 'b', Lit 'c')) in
  let simplified = simpl r in
  Printf.printf "Simplifying a*abc|bc: %s\n" (to_string simplified);
  