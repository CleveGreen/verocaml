type representation = Immediate | Value

(** [Value] is the compiler's OCaml value layout, not a guarantee that every
    inhabitant is heap allocated. Neither case determines a numeric bit width. *)
type t = private {
  representation : representation;
  compiler_jkind_abi : string;
}

(** Use the completed owning signature environment: a manifest's environment
    can retain unfinished declarations from its recursive type group. *)
val reconstruct :
  environment:Env.t -> Typedtree.type_declaration -> (t, string) result
