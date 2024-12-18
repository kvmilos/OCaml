(**************)
(* Interpreter z desugarowaniem i dodatkowym typem *)

type expr = Var of string | Int of int | Bool of bool | If of expr * expr | IfElse of expr * expr * expr;;

type expr_core = Var of string | Int of int | IfElse of expr_core * expr_core * expr_core;;


let rec desugar = function 
| Bool b -> if b then Int 1 else Int 0
| If (e1, e2) -> desugar (IfElse (e1, e2, Int 0))
| IfElse (e1, e2, e3) -> IfElse (desugar e1, desugar e2, desugar e3)
(* | e -> e to nie zadziała *)
| Int i -> Int i
| Var x -> Var x
;;

let rec eval_basic env = function
| Var s -> env s
| Int i -> i
| IfElse (e1, e2, e3) -> if eval_basic env e1 <> 0 then eval_basic env e2 else eval_basic env e3
;;

let print (e:expr) = (* match e with ... *) () ;;

let eval env e = let e1 = desugar e in print e; eval_basic env e1;;

let prog = If (Var "x", Bool true);;

let wynik = eval (fun s -> 5) prog;;


(* expr_core nie jest "podtypem" expr *)