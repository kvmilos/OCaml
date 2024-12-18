(**********************************************)
(* Polimorficzne warianty - część trudniejsza *)

(* Zwykły interpreter (bez desugarowania, na zwykłych wariantach) *)

type expr = Var of string | Int of int | Bool of bool | If of expr * expr | IfElse of expr * expr * expr;;

let rec eval env = function 
| Var s -> env s
| Int i -> i
| Bool b -> if b then 1 else 0
| If (e1, e2) -> if eval env e1 <> 0 then eval env e2 else 0
| IfElse (e1, e2, e3) -> if eval env e1 <> 0 then eval env e2 else eval env e3;;

let prog = If (Var "x", Bool true);;

let wynik = eval (fun s -> 5) prog;;