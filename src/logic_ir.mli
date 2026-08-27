type span = Diagnostic.span

type builder
type named_sort

type sort = Int | Bool | Named of named_sort

type function_symbol
type datatype
type datatype_constructor
type datatype_field
type rank_domain
type binder
type term
type user_quantifier
type axiom
type query

type feature =
  | Named_sorts
  | Uninterpreted_functions
  | Linear_integer_arithmetic
  | Quantifiers
  | Explicit_patterns
  | Quantifier_ids
  | Models
  | Nonlinear_integer_arithmetic
  | Algebraic_datatypes

type datatype_field_sort = Field_sort of sort | Recursive_self
type datatype_field_spec = {
  field_id : string;
  field_name : string;
  field_sort : datatype_field_sort;
}
type datatype_constructor_spec = {
  constructor_id : string;
  constructor_name : string;
  recognizer_id : string;
  fields : datatype_field_spec list;
}

type error = {
  span : span;
  message : string;
}

val create : unit -> builder

val declare_sort :
  builder -> name:string -> span:span -> (sort, error) result

val declare_function :
  builder ->
  name:string ->
  domain:sort list ->
  range:sort ->
  span:span ->
  (function_symbol, error) result

val declare_datatype :
  builder ->
  datatype_id:string ->
  scc_id:string ->
  sort_name:string ->
  constructors:datatype_constructor_spec list ->
  span:span ->
  (datatype, error) result

val datatype_sort : datatype -> sort
val datatype_constructors : datatype -> datatype_constructor list
val datatype_constructor_symbol : datatype_constructor -> function_symbol
val datatype_recognizer_symbol : datatype_constructor -> function_symbol
val datatype_fields : datatype_constructor -> datatype_field list
val datatype_field_symbol : datatype_field -> function_symbol

val declare_rank_domain :
  builder ->
  domain_id:string ->
  members:(string * sort) list ->
  span:span ->
  (rank_domain, error) result

val bind :
  builder -> name:string -> sort:sort -> span:span -> (binder, error) result

val int : span:span -> Z.t -> term
val bool : span:span -> bool -> term
val bound : binder -> term

val apply :
  span:span -> function_symbol -> term list -> (term, error) result

val rank_project :
  span:span ->
  rank_domain ->
  member:string ->
  term ->
  (term, error) result

val add : span:span -> term -> term -> (term, error) result
val subtract : span:span -> term -> term -> (term, error) result
val negate : span:span -> term -> (term, error) result
val scale : span:span -> Z.t -> term -> (term, error) result
val less_than : span:span -> term -> term -> (term, error) result
val less_or_equal : span:span -> term -> term -> (term, error) result
val greater_than : span:span -> term -> term -> (term, error) result
val greater_or_equal : span:span -> term -> term -> (term, error) result
val equal : span:span -> term -> term -> (term, error) result
val distinct : span:span -> term -> term -> (term, error) result
val not_ : span:span -> term -> (term, error) result
val and_ : span:span -> term list -> (term, error) result
val or_ : span:span -> term list -> (term, error) result
val implies : span:span -> term -> term -> (term, error) result

val ite :
  span:span -> term -> then_:term -> else_:term -> (term, error) result

val forall_term :
  builder ->
  binders:binder list ->
  body:term ->
  trigger:term ->
  qid:string ->
  skid:string ->
  span:span ->
  (term, error) result

val exists_term :
  builder ->
  binders:binder list ->
  body:term ->
  qid:string ->
  skid:string ->
  span:span ->
  (term, error) result

val forall :
  builder ->
  binders:binder list ->
  body:term ->
  patterns:term list list ->
  qid:string ->
  skid:string ->
  span:span ->
  (axiom, error) result

val query :
  builder ->
  axioms:axiom list ->
  assertions:term list ->
  requires:feature list ->
  span:span ->
  (query, error) result

val sort_equal : sort -> sort -> bool
val sort_to_string : sort -> string
val term_sort : term -> sort
val term_span : term -> span
val requirements : query -> feature list
val feature_to_string : feature -> string
val error_to_string : error -> string

module View : sig
  type declaration =
    | Sort_declaration of named_sort
    | Function_declaration of function_symbol

  type term_node =
    | Integer of Z.t
    | Boolean of bool
    | Bound of binder
    | Apply of function_symbol * term list
    | Rank_project of rank_domain * string * function_symbol * term
    | Add of term * term
    | Subtract of term * term
    | Negate of term
    | Scale of Z.t * term
    | Less_than of term * term
    | Less_or_equal of term * term
    | Greater_than of term * term
    | Greater_or_equal of term * term
    | Equal of term * term
    | Distinct of term * term
    | Not of term
    | And of term list
    | Or of term list
    | Implies of term * term
    | Forall_term of user_quantifier
    | Exists_term of user_quantifier
    | Ite of term * term * term

  val declarations : query -> declaration list
  val datatypes : query -> datatype list
  val axioms : query -> axiom list
  val assertions : query -> term list
  val query_span : query -> span

  val named_sort_index : named_sort -> int
  val named_sort_name : named_sort -> string
  val named_sort_span : named_sort -> span

  val function_index : function_symbol -> int
  val function_name : function_symbol -> string
  val function_domain : function_symbol -> sort list
  val function_range : function_symbol -> sort
  val function_span : function_symbol -> span

  val datatype_id : datatype -> string
  val datatype_scc_id : datatype -> string
  val datatype_sort : datatype -> named_sort
  val datatype_constructors : datatype -> datatype_constructor list
  val datatype_constructor_id : datatype_constructor -> string
  val datatype_constructor_symbol : datatype_constructor -> function_symbol
  val datatype_recognizer_symbol : datatype_constructor -> function_symbol
  val datatype_fields : datatype_constructor -> datatype_field list
  val datatype_field_id : datatype_field -> string
  val datatype_field_sort : datatype_field -> datatype_field_sort
  val datatype_field_symbol : datatype_field -> function_symbol

  val rank_domain_id : rank_domain -> string
  val rank_domain_members : rank_domain -> (string * sort) list
  val rank_domain_span : rank_domain -> span

  val binder_index : binder -> int
  val binder_name : binder -> string
  val binder_sort : binder -> sort
  val binder_span : binder -> span

  val term_node : term -> term_node
  val user_quantifier_binders : user_quantifier -> binder list
  val user_quantifier_body : user_quantifier -> term
  val user_quantifier_trigger : user_quantifier -> term option
  val user_quantifier_qid : user_quantifier -> string
  val user_quantifier_skid : user_quantifier -> string
  val user_quantifier_span : user_quantifier -> span

  val axiom_binders : axiom -> binder list
  val axiom_body : axiom -> term
  val axiom_patterns : axiom -> term list list
  val axiom_qid : axiom -> string
  val axiom_skid : axiom -> string
  val axiom_span : axiom -> span
end
