[@@@verocaml.verify]

type 'a amber_carrier = 'a option
[@@verocaml.external_type_specification]

type 'a cobalt_carrier = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) violet_carrier = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a imported_variant_catalog = 'a External_types.box
[@@verocaml.external_type_specification]

type ('a, 'error) imported_outcome_catalog =
  ('a, 'error) External_types.outcome
[@@verocaml.external_type_specification]

type 'a imported_record_catalog = 'a External_types.cell
[@@verocaml.external_type_specification]

let option_present value =
  match value with None -> false | Some _ -> true
[@@verocaml.spec]

let list_present value =
  match value with [] -> false | _ :: _ -> true
[@@verocaml.spec]

let result_present value =
  match value with Ok _ -> true | Error _ -> false
[@@verocaml.spec]

let imported_variant_present value =
  match value with External_types.Empty -> false | External_types.Box _ -> true
[@@verocaml.spec]

let imported_outcome_present value =
  match value with External_types.Good _ -> true | External_types.Bad _ -> false
[@@verocaml.spec]

let imported_record_flag value = value.External_types.flag
[@@verocaml.spec]

let construct_option value =
  [%verocaml.ensures fun result -> option_present result];
  Some value

let construct_list value =
  [%verocaml.ensures fun result -> list_present result];
  [value]

let construct_result value =
  [%verocaml.ensures fun result -> result_present result];
  Ok value

let construct_imported_variant value =
  [%verocaml.ensures fun result -> imported_variant_present result];
  External_types.Box value

let construct_imported_outcome value =
  [%verocaml.ensures fun result -> imported_outcome_present result];
  External_types.Good value

let construct_imported_record value =
  [%verocaml.ensures fun result -> imported_record_flag result];
  { External_types.value; flag = true }
