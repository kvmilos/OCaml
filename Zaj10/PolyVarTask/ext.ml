open Basic

type 'a ext_expr = [ `Bool of bool | `If of 'a * 'a * 'a | 'a basic_expr ]
type ext_expression = ext_expression ext_expr

type ext_instr = [ `If | basic_instr ]

(* Check if an ext_expression is purely basic (no If/Bool) *)
let rec is_pure_basic (e: ext_expression) : bool =
  match e with
  | `Int _ -> true
  | `Var _ -> true
  | `Add (x, y) -> is_pure_basic x && is_pure_basic y
  | `Bool _ -> false
  | `If _ -> false

(* Convert an ext_expression that we know is purely basic to a basic_expression *)
let rec to_basic_expr (e : ext_expression) : basic_expression =
  match e with
  | `Int i -> `Int i
  | `Var s -> `Var s
  | `Add (x, y) -> `Add (to_basic_expr x, to_basic_expr y)
  | `Bool _ | `If _ ->
      failwith "Unexpected `If` or `Bool` in a supposedly pure basic expression"

let rec eval_ext env (e : ext_expression) : int =
  match e with
  | `Bool b -> if b then 1 else 0
  | `If (e1, e2, e3) ->
     let cond = eval_ext env e1 in
     if cond = 0 then eval_ext env e3 else eval_ext env e2
  | #basic_expr as b ->
     if is_pure_basic b then
       eval_basic env (to_basic_expr b)
     else
       (* Evaluate manually using ext rules, since it is not purely basic *)
       match b with
       | `Int i -> i
       | `Var s -> env s
       | `Add (x, y) -> eval_ext env x + eval_ext env y

let rec string_of_ext_expr (e : ext_expression) : string =
  match e with
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `If (e1, e2, e3) ->
     "if " ^ string_of_ext_expr e1 ^
     " then " ^ string_of_ext_expr e2 ^
     " else " ^ string_of_ext_expr e3
  | #basic_expr as b ->
     if is_pure_basic b then
       string_of_basic_expr (to_basic_expr b)
     else
       (* Manually convert to string *)
       match b with
       | `Int i -> string_of_int i
       | `Var s -> s
       | `Add (x, y) -> "(" ^ string_of_ext_expr x ^ " + " ^ string_of_ext_expr y ^ ")"

let print_ext_expr e =
  print_endline (string_of_ext_expr e)

let rec translate_ext (e : ext_expression) : [> ext_instr] list =
  match e with
  | `Bool b -> [`Int (if b then 1 else 0)]
  | `If (e1, e2, e3) ->
     translate_ext e1 @ translate_ext e2 @ translate_ext e3 @ [`If]
  | #basic_expr as b ->
     if is_pure_basic b then
       translate_basic (to_basic_expr b)
     else
       (* Manually translate *)
       match b with
       | `Int i -> [`Int i]
       | `Var s -> [`Var s]
       | `Add (x, y) -> translate_ext x @ translate_ext y @ [`Add]

let run_ext_instr env stck = function
  | `If ->
     (* Stack: top: else_val, next: then_val, next: cond *)
     let else_val = List.hd stck in
     let then_val = List.hd (List.tl stck) in
     let cond = List.hd (List.tl (List.tl stck)) in
     let rest = List.tl (List.tl (List.tl stck)) in
     if cond = 0 then else_val :: rest else then_val :: rest
  | #basic_instr as bi -> run_basic_instr env stck bi

let run_ext env prg =
  let prg = (prg :> ext_instr list) in
  let stck = List.fold_left (run_ext_instr env) [] prg in
  List.hd stck

let string_of_ext_instr = function
  | `If -> "If"
  | #basic_instr as bi -> string_of_basic_instr bi

let print_ext_prog prg =
  let prg = (prg :> ext_instr list) in
  List.iter (fun instr -> print_endline (string_of_ext_instr instr)) prg

let check_ext e =
  let env = fun _ -> 5 in
  let wynik = eval_ext env e in
  let prg = translate_ext e in
  print_endline "Program (ext):";
  print_ext_expr e;
  print_endline "Eval:";
  print_int wynik; print_newline ();
  print_endline "Translation:";
  print_ext_prog prg;
  print_endline "Run:";
  let wyn = run_ext env prg in
  print_int wyn; print_newline ()
