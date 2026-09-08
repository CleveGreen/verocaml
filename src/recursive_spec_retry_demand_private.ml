type t = {
  application : Vir.aggregate_term;
  callee : Sst.function_id;
  arguments : Vir.recursive_spec_argument list;
  result_type : Vir.aggregate_type;
  application_identity : Recursive_spec_application_identity.t;
}

let direct = function
  | {
      Vir.aggregate_desc =
        Vir.Aggregate_recursive_spec_application
          {
            callee;
            arguments;
            result_type;
            span;
            application_identity;
            _;
          };
      aggregate_type;
    } as application ->
      if
        result_type <> aggregate_type
        || not
             (Recursive_spec_application_identity.authenticate
                application_identity)
      then
        Error
          (`Malformed
            (span, "aggregate recursive application has forged identity or sort"))
      else
        Ok
          (Some
             {
               application;
               callee;
               arguments;
               result_type;
               application_identity;
             })
  | _ -> Ok None

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let rec of_goal = function
  | Vir.Forall_term _ | Vir.Exists_term _ -> Ok None
  | Vir.Aggregate_equal (left, right) ->
      let* left = direct left in
      let* right = direct right in
      (match (left, right) with
      | Some application, None | None, Some application ->
          Ok (Some application)
      | None, None | Some _, Some _ -> Ok None)
  | Vir.Parametric_equal (left, right) ->
      let conditions =
        Parametric_logic_private.conditions left
        @ Parametric_logic_private.conditions right
      in
      List.fold_left
        (fun result condition ->
          let* found = result in
          match found with Some _ -> Ok found | None -> of_goal condition)
        (Ok None) conditions
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Boolean_not _
  | Vir.Boolean_and _ | Vir.Boolean_or _ | Vir.Integer_compare _
  | Vir.Boolean_equal _ | Vir.Boolean_not_equal _ | Vir.Boolean_selector _
  | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _
  | Vir.Boolean_invariant_application _
  | Vir.Logical_adt_schema _
  | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_specification_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
      Ok None

let application demand = demand.application
let callee demand = demand.callee
let arguments demand = demand.arguments
let result_type demand = demand.result_type
let application_identity demand = demand.application_identity
