(* GADTs *)
(* Generalized Algebraic Data Types *)


(* Dotychczas w polimorficznych typach wariantowych zmienne typowe występowały wszędzie takie same, np: *)

type 'a drzewo = Puste | Wezel of 'a drzewo * 'a * 'a drzewo

(* W GADTach tak wcale być nie musi (dlatego generalized), np: *)

type 'a typed_expr = 
| Var : string -> int typed_expr
| Int : int -> int typed_expr
| Bool : bool -> bool typed_expr
| IfElse : bool typed_expr * int typed_expr * int typed_expr -> int typed_expr
| Eq : 'b typed_expr * 'b typed_expr -> bool typed_expr 
;;

let tprog = IfElse (Eq (Int 1, Int 0), Int 42, Int 17)

(* let bad_tprog = IfElse (Bool true, Int 42, Bool false);; *)

let rec eval : type t. (string -> int) -> t typed_expr -> t = fun env ex -> match ex with  
| Var v -> env v
| Int i -> i
| Bool b -> b
| IfElse (c, t, e) -> if eval env c then eval env t else eval env e
| Eq (e1, e2) -> (eval env e1 = eval env e2) 
;;

let w = eval (fun _ -> 42) tprog;;

(* transformacja wartości nietypowanej w typowaną jest troszkę dziwna... *)

type expr = Var of string | Int of int | Bool of bool | Eq of expr * expr | IfElse of expr * expr * expr;;

(* proste podejście nie działa *)
(* let rec typecheck : type t. expr -> t typed_expr = fun e -> match[@warning "-8"] e with
| Var v -> Var v
| Bool b -> Bool b
;;
 *)
(* musimy mieć reprezentację typów jako wartości, ale jednocześnie taką, żeby wiązała zmienne typowe *)
(* do tego właśnie służą GADTy *)
type 'a typ = TInt : int typ | TBool : bool typ;;

(* tak też za łatwo... *)
(* let rec typecheck : type t. expr -> t typ * t typed_expr = fun e -> match[@warning "-8"] e with
| Var v -> (TInt, Var v)
;;
 *)
(* musimy spakować typ i wynik (w kolejnym GADTcie) i potem dopiero go wypakować *)
type dyn = Dyn : 'a typ * 'a typed_expr -> dyn;; 

exception TypeError;;

let rec typecheck : expr -> dyn = fun e -> match e with
| Var v -> Dyn (TInt, Var v)
| Int i -> Dyn (TInt, Int i)
| Bool b -> Dyn (TBool, Bool b)
| IfElse (c, t, e) -> (match typecheck c, typecheck t, typecheck e with
    | Dyn(TBool, typed_c), Dyn(TInt, typed_t), Dyn(TInt, typed_e) ->
        Dyn(TInt, IfElse (typed_c, typed_t, typed_e))
    | _ -> raise TypeError)
| Eq (e1, e2) -> (match typecheck e1, typecheck e2 with 
    | Dyn(TBool, typed_e1), Dyn(TBool, typed_e2) -> Dyn(TBool, Eq (typed_e1, typed_e2))
    | Dyn(TInt, typed_e1), Dyn(TInt, typed_e2) -> Dyn(TBool, Eq (typed_e1, typed_e2))
    | _ -> raise TypeError)
;;    


let get_typed_expr : type t. t typ -> expr -> t typed_expr = fun t e -> match t, typecheck e with
  (* | a, Dyn(b, texpr) when a = b -> texpr (* to nie działa *) *)
  | TInt, Dyn(TInt, texpr) -> texpr
  | TBool, Dyn(TBool, texpr) -> texpr
  | _ -> raise TypeError


let prog2 = IfElse (Eq (Int 1, Int 0), Int 42, Int 17);;

let tprog2 = get_typed_expr TInt prog2;;

let w2 = eval (fun _ -> 5) tprog2;;

let prog3 = IfElse (Eq (Bool true, Bool false), Int 42, Int 17);;

let tprog3 = get_typed_expr TInt prog3;;

let w3 = eval (fun _ -> 8) tprog3;;

let bad_prog = IfElse (Bool true, Int 42, Bool false);;

let bad_tprog = get_typed_expr TInt bad_prog;;

