(**************)
(* Interpreter z desugarowaniem - zwykłe warianty - wersja bez dodatkowych typów *)

type expr = Var of string | Int of int | Bool of bool | If of expr * expr | IfElse of expr * expr * expr;;

let desugar = function 
| Bool b -> Int (if b then 1 else 0)
| If (e1, e2) -> IfElse (e1, e2, Int 0)
| e -> e
;;

let rec eval_basic env = function (* [@warning "-8"] *)
| Var s -> env s
| Int i -> i
| IfElse (e1, e2, e3) -> if eval_basic env e1 <> 0 then eval_basic env e2 else eval_basic env e3
;;

let eval env expr = let expr1 = desugar expr in eval_basic env expr1;;

let prog = If (Var "x", Bool true);;

let wynik = eval (fun s -> 5) prog;;




















































(* To po prostu nie działa i kompilator nas przed tym nie ostrzegł - lipa!*)
