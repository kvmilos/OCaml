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
    | _ -> Or(sr1, sr2) (* a|b = a|b *) end
| (Concat(r1, r2)) -> begin
  let sr1 = simpl r1 in
  let sr2 = simpl r2 in
  match (sr1, sr2) with
  | (Eps, _) -> simpl r2 (* εa = a *)
  | (_, Eps) -> simpl r1 (* aε = a *)
  | (_, _) -> Concat(simpl r1, simpl r2) (* (a|a)a = aa *) end
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
    let r1_wrapped = match r1 with Or _ -> "(" ^ r1_str ^ ")" | _ -> r1_str in
    let r2_wrapped = match r2 with Or _ -> "(" ^ r2_str ^ ")" | _ -> r2_str in
    r1_wrapped ^ r2_wrapped
  | Star r ->
    let r_str = repr f r in
    if match r with Or _ -> true | _ -> false then "(" ^ r_str ^ ")*"
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
  (* dobrze *)
  Printf.printf "der 'a' (a|b)*: %s\n" (to_string (der 'a' r2));
  (* dobrze *)
  Printf.printf "der 'a' a*aa: %s\n" (to_string (der 'a' r3));
  (* dobrze *)
  Printf.printf "der 'a' aa*bc: %s\n" (to_string (der 'a' r4));
  (* dobrze *)
  Printf.printf "der 'a' a*abc: %s\n" (to_string (der 'a' r5));
  (* źle *)

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Lit 'a')), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying a*aa|a: %s\n" (to_string simplified);
  (* dobrze *)

  let r = Or(Or(Lit 'a', Lit 'b'), Lit 'b') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)|b: %s\n" (to_string simplified);
  (* dobrze *)

  let r = Or(Star(Lit 'a'), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying a*|a: %s\n" (to_string simplified);
  (* dobrze *)

  let r = Or(Concat(Star(Lit 'a'), Lit 'b'), Lit 'b') in
  let simplified = simpl r in
  Printf.printf "Simplifying a*b|b: %s\n" (to_string simplified);
  (* źle *)
  
  let r = Or(Concat(Star(Lit 'a'), Lit 'b'), Concat(Lit 'a', Lit 'b')) in
  let simplified = simpl r in
  Printf.printf "Simplifying a*b|ab: %s\n" (to_string simplified);
  (* źle *)

  let r = Or(Concat(Star(Lit 'a'), Concat(Lit 'a', Concat(Lit 'b', Lit 'c'))), Concat(Lit 'b', Lit 'c')) in
  let simplified = simpl r in
  Printf.printf "Simplifying a*abc|bc: %s\n" (to_string simplified);
  (* źle *)

  let r = Star(Star(Lit 'a')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a*)*: %s\n" (to_string simplified);
  (* dobrze *)

  let r = Or(Star(Or(Lit 'a', Lit 'b')), Lit 'a') in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)*|a: %s\n" (to_string simplified);
  (* źle *)

  let r = Or(Star(Or(Lit 'a', Lit 'b')), Or(Lit 'a', Lit 'b')) in
  let simplified = simpl r in
  Printf.printf "Simplifying (a|b)*|(a|b): %s\n" (to_string simplified);
  (* dobrze *)