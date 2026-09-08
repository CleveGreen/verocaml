type kind = Bounds | Checked | Partial | Modular | Operation
type t = private {
  kind : kind;
  declaration : Numeric_semantics_correlation_private.t;
  view : Sst.function_definition;
  clause : Sst.ensures_clause;
  relation : Sst.expression;
  required_guards : Sst.predicate_clause list;
  material : string;
}
val kind_name : kind -> string
val match_relation :
  kind:kind -> base_full_key:string -> target:Build_target_profile_private.instance ->
  semantic_width:int -> signed:bool -> view:Sst.function_definition ->
  bounds:Sst.function_definition option ->
  Numeric_semantics_correlation_private.t -> t option
