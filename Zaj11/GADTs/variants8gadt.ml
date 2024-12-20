(* GADTs *)
(* Generalized Algebraic Data Types *)


(* Dotychczas w polimorficznych typach wariantowych zmienne typowe występowały wszędzie takie same, np: *)

type 'a drzewo = Puste | Wezel of 'a drzewo * 'a * 'a drzewo

(* W GADTach tak wcale być nie musi, np: *)

type 'a typed_expr = 
| Var : string -> int typed_expr
| Int : int -> int typed_expr
| Bool : bool -> bool typed_expr
| IfElse : bool typed_expr * int typed_expr * int typed_expr -> int typed_expr
| Eq : 'b typed_expr * 'b typed_expr -> bool typed_expr 
;;

let tprog = IfElse (Eq (Int 1, Int 0), Int 42, Int 17)

(* let bad_tprog = IfElse (Bool true, Int 42, Bool false);; *)

let rec eval : type a. (string -> int) -> a typed_expr -> a = fun env ex -> match ex with  
| Var v -> env v
| Int i -> i
| Bool b -> b
| IfElse (c, t, e) -> if eval env c then eval env t else eval env e
| Eq (e1, e2) -> (eval env e1 = eval env e2) 
;;

let w = eval (fun _ -> 42) tprog;;

(* transformacja wartości nietypowanej w typowaną jest troszkę dziwna... *)

type expr = Var of string | Int of int | Bool of bool | Eq of expr * expr | IfElse of expr * expr * expr;;

(* musimy zgadnąć typ *)
type 'a typ = TInt : int typ | TBool : bool typ;;

exception TypeError;;

let rec typecheck : type t. expr -> t typ -> t typed_expr = fun e t -> match e, t with
| Var v, TInt -> (Var v)
| Int i, TInt -> (Int i)
| Bool b, TBool -> (Bool b)
| Eq (e1, e2), TBool -> 
    (try (* może dwa inty ?*)
      let typed_e1 = typecheck e1 TInt in
      let typed_e2 = typecheck e2 TInt in
      Eq (typed_e1, typed_e2)
    with 
      TypeError -> (* jak nie to może dwa boole ? *)
        let typed_e1 = typecheck e1 TBool in
        let typed_e2 = typecheck e2 TBool in
        Eq (typed_e1, typed_e2))
| IfElse (c, t, e), TInt ->
    let typed_c = typecheck c TBool in
    let typed_t = typecheck t TInt in
    let typed_e = typecheck e TInt in
    IfElse (typed_c, typed_t, typed_e)
| _ -> raise TypeError
;;    


let prog2 = IfElse (Eq (Int 1, Int 0), Int 42, Int 17);;

let tprog2 = typecheck prog2 TInt;;

let w2 = eval (fun _ -> 5) tprog2;;

let prog3 = IfElse (Eq (Bool true, Bool false), Int 42, Int 17);;

let tprog3 = typecheck prog3 TInt;;

let w3 = eval (fun _ -> 8) tprog3;;

let bad_prog = IfElse (Bool true, Int 42, Bool false);;

let bad_tprog = typecheck bad_prog TInt;;

