type representation = Immediate | Boxed

(** Author-written compatibility requests only.  These fields never constitute
    compiler, provider, semantic-law, runtime-refinement, or target-profile
    authority. *)
type carrier = private {
  base_path : string option;
  profile_reference : string;
  representation : representation;
  compatibility : string list;
}

type role = private {
  carrier_path : string;
  role_schema : string;
  role_identity : string;
  semantics_path : string;
  visibility : string;
  reveal : bool;
  inline : bool;
}

val carrier_marker : string
val role_marker : string
val parse_carrier : Parsetree.attribute -> (carrier, string) result
val parse_role : Parsetree.attribute -> (role, string) result
val carrier_material : carrier -> string
val role_material : role -> string
