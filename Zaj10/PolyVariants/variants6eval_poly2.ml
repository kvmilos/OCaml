(**************)
(* Interpreter z desugarowaniem - polimorficzne warianty z podziałem na nazwane kawałki... *)

type basic_literal = [`Var of string | `Int of int ];;

type 'expr cond = [ `IfElse of 'expr * 'expr * 'expr ];;

(*0*)
type basic_expr = [ basic_literal | basic_expr cond ];;

(*2*)
(* type 'a basic_expr = [> basic_literal | 'a cond ] as 'a;; *)


type ext_literal = [ basic_literal | `Bool of bool ];;

type 'expr ext_cond = [ 'expr cond | `If of 'expr * 'expr ];;

(*0*)
type ext_expr = [ ext_literal | ext_expr ext_cond ];;

(*3*)
(* type 'a ext_expr = [> ext_literal | 'a ext_cond ] as 'a;; *)


(*0*)
let rec desugar (e : ext_expr) (* : basic_expr *) = 

(*1*)
(* let rec desugar (e : ext_expr) : [> basic_literal | 'a cond ] as 'a = *)

(*2*)
(* let rec desugar (e : ext_expr) : 'a basic_expr = *)

(*3*)
(* let rec desugar (e : 'a ext_expr) : 'b basic_expr = *)
match e with 
| `Bool b -> if b then `Int 1 else `Int 0
| `If (e1, e2) -> desugar (`IfElse (e1, e2, `Int 0))
| `IfElse (e1, e2, e3) -> `IfElse (desugar e1, desugar e2, desugar e3)
| #basic_literal as e -> e

let rec eval_core env e = match e with
| `Var s -> env s
| `Int i -> i 
| `IfElse (e1, e2, e3) -> if eval_core env e1 <> 0 then eval_core env e2 else eval_core env e3

let print (e: ext_expr) = (* match e with .... *) ();; 

let eval env e = let e1 = desugar e in print e1; eval_core env e1;;

let prog = `If (`Var "x", `Bool true);;

let wynik = eval (fun s -> 5) prog;;


(*
Jak napisać specyfikację typu wyjściowego desugar?
0 - bez niczego działa! Dobre i to! No ale my właśnie chcieliśmy dopisać typy!
  - z " : basic_expr" nie działa: basic_expr i ext_expr się nie unifikują - to są różne typy :(
  - z " : [ > basic_expr ]" też nie, bo to się unifikuje z typem, który mu sam wychodzi i właśnie mu wychodzi, że to jest _dokładnie_
    basic_expr, a wcale nie "co najmniej" basic_expr...

1 - jak się dopisze anonimową definicję niby równą basic_expr, to działa!

Ale my tam chcemy nazwę wpisać! A nie "anonimowe coś"!
No to może zmienić definicję? Jak? Dopisać > ? Nie! Po prawej jest luz (zmienna typowa), po lewej jej nie ma - źle.
No to musi być!

2 - Mniej więcej nazywamy to, co pisaliśmy anonimowo w typie desugar
    zachowując luz (zmienną typową)

3 - Dla elegancji (i w trosce o ew. przyszłe rozszerzenia) to samo można by zrobić z ext_expr - wtedy też działa!
    (trzeba poprawić też oczywiście print)

*)
