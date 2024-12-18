(***********************************************)
(* Interpreter and compiler - extended version *)

open Basic

(* Type of extended expressions *)
type 'a ext_expr = [ `Bool of bool | `If of 'a * 'a * 'a | 'a basic_expr ]
type ext_expression = ext_expression ext_expr

(* Type of extended Stack Machine instructions *)
type ext_instr = [ `If | basic_instr ]

(* Evaluation and printing *)
val eval_ext : env -> ext_expression -> int
val string_of_ext_expr : ext_expression -> string
val print_ext_expr : ext_expression -> unit

(* Translation to (extended) Stack Machine instructions *)
val translate_ext : ext_expression -> [> ext_instr] list

(* Running and printing of (extended) Stack Machine instructions *)
val run_ext_instr : env -> int list -> ext_instr -> int list
val run_ext : env -> ext_instr list -> int
val string_of_ext_instr : ext_instr -> string
val print_ext_prog : ext_instr list -> unit

(* Run them all: evaluation, translation, SM run *)
val check_ext : ext_expression -> unit
