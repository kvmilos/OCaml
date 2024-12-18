(********************************************)
(* Interpreter and compiler - basic version *)

(* Type of (extensible) basic expressions *)
type 'a basic_expr = [ `Int of int | `Var of string | `Add of 'a * 'a ]
type basic_expression = basic_expression basic_expr

(* Type of basic Stack Machine instructions *)
type basic_instr = [ `Int of int | `Var of string | `Add ]

(* Variable valuation: from variable names to integers *)
type env = string -> int

(* Evaluation and printing *)
val eval_basic : env -> basic_expression -> int
val string_of_basic_expr : basic_expression -> string
val print_basic_expr : basic_expression -> unit

(* Translation to Stack Machine instructions *)
val translate_basic : basic_expression -> [> basic_instr] list

(* Running and printing of Stack Machine instructions *)
val run_basic_instr : env -> int list -> basic_instr -> int list
val run_basic : env -> basic_instr list -> int
val string_of_basic_instr : basic_instr -> string
val print_basic_prog : basic_instr list -> unit

(* Run them all: evaluation, translation, SM run *)
val check_basic : basic_expression -> unit

