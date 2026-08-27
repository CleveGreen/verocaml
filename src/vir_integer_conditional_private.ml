let issued_conditionals : Vir.integer_term Weak.t list ref = ref []

let register term =
  let issued = Weak.create 1 in
  Weak.set issued 0 (Some term);
  issued_conditionals := issued :: !issued_conditionals; term

let create ~condition ~consequent ~alternative =
  match condition with
  | Vir.Aggregate_equal (left, right)
    when left.aggregate_type = right.aggregate_type ->
      Vir.Integer_conditional (condition, consequent, alternative) |> register
  | Vir.Aggregate_equal _ | Vir.Boolean_constant _ | Vir.Boolean_symbol _
  | Vir.Forall_term _ | Vir.Exists_term _
  | Vir.Boolean_not _ | Vir.Boolean_and _ | Vir.Boolean_or _
  | Vir.Boolean_equal _ | Vir.Boolean_not_equal _ | Vir.Integer_compare _
  | Vir.Boolean_selector _ | Vir.Boolean_invariant_application _
  | Vir.Logical_adt_schema _
  | Vir.Parametric_equal _ | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_specification_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
      invalid_arg
        "integer conditional requires same-sort aggregate location equality"

let create_formula ~condition ~consequent ~alternative =
  Vir.Integer_conditional (condition, consequent, alternative) |> register
let authenticate term =
  let live, authenticated =
    List.fold_left
      (fun (live, authenticated) issued ->
        match Weak.get issued 0 with
        | None -> (live, authenticated)
        | Some candidate ->
            (issued :: live, authenticated || candidate == term))
      ([], false) !issued_conditionals
  in
  issued_conditionals := List.rev live;
  authenticated
