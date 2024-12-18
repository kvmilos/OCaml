(********************************************)
(* Interpreter and compiler - basic version *)

type 'a basic_expr = [ `Int of int | `Var of string | `Add of 'a * 'a ]
type basic_expression = basic_expression basic_expr

type basic_instr = [ `Int of int | `Var of string | `Add ]

type env = string -> int

let rec eval_basic env = function
| `Var s -> env s
| `Int i -> i
| `Add (e1, e2) -> eval_basic env e1 + eval_basic env e2

let rec string_of_basic_expr = function  
| `Var s -> s
| `Int i -> string_of_int i
| `Add (e1, e2) -> "("^string_of_basic_expr e1^" + "^string_of_basic_expr e2^")"

let print_basic_expr e = 
  print_endline (string_of_basic_expr e)


let rec translate_basic = function
| `Int _ 
| `Var _ as e -> [e]
| `Add (e1, e2) -> translate_basic e1 @ translate_basic e2 @ [`Add]


let run_basic_instr env stck = function 
| `Int i -> i::stck
| `Var s -> env s :: stck
| `Add -> List.(hd stck + hd (tl stck) :: tl (tl stck)) 

let run_basic env prg = 
  let stck = List.fold_left (run_basic_instr env) [] prg in
  List.hd stck


let string_of_basic_instr = function
| `Var s -> "Var "^s
| `Int i -> "Push "^string_of_int i
| `Add -> "Add"

let print_basic_prog prg = 
  List.iter (fun instr -> print_endline (string_of_basic_instr instr)) prg


let check_basic expr = 
  let env = fun _ -> 5 in
  let wynik = eval_basic env expr in
  let prg = translate_basic expr in
  let wyn = run_basic env prg in
  print_endline "Program (basic):";
  print_basic_expr expr; 
  print_endline "Eval:";
  print_int wynik; print_newline ();
  print_endline "Translation:";
  print_basic_prog prg;
  print_endline "Run:";
  print_int wyn; print_newline ()
