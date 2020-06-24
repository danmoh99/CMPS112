(* Generated file: DO NOT EDIT *)
(* ocamlc -i tables.ml *)
(* Generated Sat Feb  2 17:54:32 PST 2019 *)

type variable_table_t = (string, float) Hashtbl.t
val variable_table : variable_table_t

type array_table_t = (string, float array) Hashtbl.t
val array_table : array_table_t

type unary_fn_table_t = (string, float -> float) Hashtbl.t
val unary_fn_table : unary_fn_table_t

type binary_fn_table_t = (string, float -> float -> float) Hashtbl.t
val binary_fn_table : binary_fn_table_t

type label_table_t = (string, Absyn.program) Hashtbl.t
val label_table : label_table_t
val init_label_table : Absyn.program -> unit
val dump_label_table : unit -> unit
