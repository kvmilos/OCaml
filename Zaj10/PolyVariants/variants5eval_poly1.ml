(**************)
(* Interpreter z desugarowaniem - polimorficzne warianty *)

type expr = [ `Var of string | `Int of int | `Bool of bool | `If of expr * expr | `IfElse of expr * expr * expr ];;

let rec desugar (e:expr) = match e with 
| `Bool b -> if b then `Int 1 else `Int 0
| `If (e1, e2) -> desugar (`IfElse (e1, e2, `Int 0))
| `IfElse (e1, e2, e3) -> `IfElse (desugar e1, desugar e2, desugar e3)
| `Var _ 
| `Int _ as e -> e

(* | `Var s -> `Var s    (* też ok oczywiście*)
| `Int i -> `Int i
*)

(* | e -> e  (* nie ok!*)
*)

let rec eval_basic env = function
| `Var s -> env s
| `Int i -> i
| `IfElse (e1, e2, e3) -> if eval_basic env e1 <> 0 then eval_basic env e2 else eval_basic env e3
;;

let print (e:expr) = (* match e with .... *) ();; 

let eval env e = let e1 = desugar e in print e1; eval_basic env e1;;

let prog = `If (`Var "x", `Bool true);;

let wynik = eval (fun s -> 5) prog;;


(* a gdyby tak teraz zapomnieć wywołania rek. desugar ? *)
