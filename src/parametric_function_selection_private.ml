type kind = Exec | Spec | Recursive_spec | Proof | Other

let contains signature predicate =
  List.exists
    (Parametric_lowering_private.contains_source_application
       ~is_application:predicate)
    signature

let eligible ~kind ~recursive ~type_variables ~signature =
  let non_scalar_application =
    contains signature (fun path ->
        not
          (Path.same path Predef.path_unit
          || Path.same path Predef.path_bool
          || Path.same path Predef.path_int))
  in
  type_variables <> []
  && signature <> []
  && List.for_all Parametric_lowering_private.first_order_source_type signature
  &&
  match kind with
  | Exec | Spec -> true
  | Recursive_spec | Proof -> (not recursive) || non_scalar_application
  | Other -> true
