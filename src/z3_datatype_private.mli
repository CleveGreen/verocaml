(** Worker-context-local reconstruction of portable Logic IR datatypes. *)

type bindings = {
  sorts : (int * Z3.Sort.sort) list;
  functions : (int * Z3.FuncDecl.func_decl) list;
}

type sort_reference =
  | Int_sort
  | Bool_sort
  | Named_sort of int
  | Recursive_self

type field = {
  field_function_index : int;
  field_function_name : string;
  field_sort : sort_reference;
}

type constructor = {
  constructor_function_index : int;
  constructor_function_name : string;
  recognizer_function_index : int;
  recognizer_function_name : string;
  fields : field list;
}

type declaration = {
  sort_index : int;
  sort_name : string;
  constructors : constructor list;
}

val detach : Logic_ir.datatype -> declaration

val declare :
  context:Z3.context ->
  resolve_sort:(Logic_ir.sort -> Z3.Sort.sort) ->
  Logic_ir.datatype ->
  (bindings, string) result

val declare_detached :
  context:Z3.context ->
  resolve_named_sort:(int -> Z3.Sort.sort) ->
  declaration ->
  (bindings, string) result
