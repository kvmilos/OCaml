open SimpleRegexpDef

type 'c reg = 'c SimpleRegexpDef.reg

let rec simpl = function
  | Or(r, Empty) | Or(Empty, r) -> simpl r (* a|∅ = ∅|a = a *)
  | Concat(Eps, r) | Concat(r, Eps) -> simpl r (* εa = aε = a *)
  | Or(r1, r2) ->
    let sr1 = simpl r1 in
    let sr2 = simpl r2 in
    if sr1 = sr2 then sr1 (* a|a = a *)
    else if sr1 = Star(sr2) || sr2 = Star(sr1) then Star sr1 (* a|a* = a*|a = a* *)
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
  | Or(r1, r2) -> Or(der c r1, der c r2) (* (a|b) -> ε|∅ *)
  | Concat(r1, r2) -> 
    let part1 = Concat(der c r1, r2) in
    let part2 = if nullable r1 then der c r2 else Empty 
  in Or(part1, part2) (* DO DOPRACOWANIA *)
  | Star r1 ->
    let part1 = Concat(r1, Star r1) in
    let part2 = if nullable r1 then Star r1 else Empty
  in Concat(der c r1, Or(part1, part2)) (* a* -> aa*|a* *) (* DO DOPRACOWANIA *)


let ders cs r =
  List.fold_left (fun acc c -> der c acc) r cs

let accept r cs =
    let r' = ders cs r in
    nullable r' (* DO DOPRACOWANIA *)
  
let rec repr f = function
  | Empty -> "∅"
  | Eps -> "ε"
  | Lit c -> f c
  | Or(r1, r2) -> "(" ^ repr f r1 ^ "|" ^ repr f r2 ^ ")"
  | Concat(r1, r2) -> "(" ^ repr f r1 ^ repr f r2 ^ ")"
  | Star r -> "(" ^ repr f r ^ ")*"
  
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