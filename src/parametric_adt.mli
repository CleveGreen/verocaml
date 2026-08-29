type provenance =
  | Local of { compiler_uid : string }
  | External of {
      compiler_uid : string;
      proxy_uid : string;
      prelude : bool;
    }

type field = {
  field_index : int;
  field_name : string;
  field_uid : string;
  field_type : Parametric_type.t;
  field_mutable : bool;
}

type constructor = {
  constructor_index : int;
  constructor_name : string;
  constructor_uid : string;
  constructor_fields : field list;
}

type kind = Record of field list | Variant of constructor list

type t

type scalar_kind = Scalar_bool | Scalar_int

type option_instance = {
  option_descriptor : t;
  option_arguments : Parametric_type.t list;
  option_payload_type : Parametric_type.t;
  option_absent : constructor;
  option_present : constructor;
}

type error = { descriptor : string; message : string }

val create :
  type_id:Parametric_type.type_id ->
  type_constructor:Parametric_type.constructor ->
  binders:Parametric_type.binder list ->
  provenance:provenance ->
  kind:kind ->
  (t, error) result

val type_id : t -> Parametric_type.type_id
val type_constructor : t -> Parametric_type.constructor
val binders : t -> Parametric_type.binder list
val provenance : t -> provenance
val kind : t -> kind
val recursive_fields : t -> (int option * field) list
val authenticates_recursive_field :
  t -> constructor_index:int option -> field_index:int -> bool
val compiler_uid : t -> string
val is_standard : t -> bool
val validate_registry : t list -> (unit, error) result
val find : t list -> Parametric_type.constructor -> t option
val option_instance : t list -> Parametric_type.t -> option_instance option
val instantiate_field : t -> Parametric_type.t list -> field -> (Parametric_type.t, string) result
val instantiate_field_by_index :
  t ->
  Parametric_type.t list ->
  constructor_index:int option ->
  field_index:int ->
  (Parametric_type.t, string) result
val same_application : t -> Parametric_type.t -> bool
val application : t -> Parametric_type.t list -> (Parametric_type.t, string) result
val deeply_immutable_instance : t list -> Parametric_type.t -> bool
val exec_scalar_layout :
  t list ->
  Parametric_type.t ->
  (constructor * (field * scalar_kind) list) list option
val exec_scalar_equality : t list -> Parametric_type.t -> bool
val to_string : t -> string
