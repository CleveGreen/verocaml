let type_label aggregate =
  let arguments =
    aggregate.Vir.aggregate_type_arguments
    |> List.map Parametric_type.to_string |> String.concat ","
  in
  Printf.sprintf "%s#%d<%s>" aggregate.aggregate_type_name
    aggregate.aggregate_type_index arguments

let suffix aggregate =
  type_label aggregate |> Digest.string |> Digest.to_hex
  |> fun digest -> String.sub digest 0 12

let tag aggregate =
  if aggregate.Vir.aggregate_type_arguments = [] then
    Printf.sprintf "verocaml_tag_t%d_%s" aggregate.aggregate_type_index
      aggregate.aggregate_type_name
  else
    Printf.sprintf "verocaml_tag_t%d_%s_%s" aggregate.aggregate_type_index
      aggregate.aggregate_type_name (suffix aggregate)

let constructor aggregate (constructor : Sst.constructor_id) =
  if aggregate.Vir.aggregate_type_arguments = [] then
    Printf.sprintf "verocaml_ctor_t%d_%s_c%d_%s"
      constructor.constructor_type.type_index
      constructor.constructor_type.type_name constructor.constructor_index
      constructor.constructor_name
  else
    Printf.sprintf "verocaml_ctor_t%d_%s_%s_c%d_%s"
      constructor.constructor_type.type_index
      constructor.constructor_type.type_name (suffix aggregate)
      constructor.constructor_index constructor.constructor_name

let record_constructor aggregate (record_type : Sst.type_id) =
  if aggregate.Vir.aggregate_type_arguments = [] then
    Printf.sprintf "verocaml_record_ctor_t%d_%s" record_type.type_index
      record_type.type_name
  else
    Printf.sprintf "verocaml_record_ctor_t%d_%s_%s" record_type.type_index
      record_type.type_name (suffix aggregate)

let selector ~range (selector : Vir.selector) =
  let path =
    selector.selector_path |> List.map string_of_int |> String.concat "_"
  in
  if selector.selector_domain.aggregate_type_arguments = [] then
    Printf.sprintf "verocaml_sel_t%d_%s_i%d_%s_p%s_r%s"
      selector.selector_domain.aggregate_type_index selector.selector_namespace
      selector.selector_index selector.selector_name path range
  else
    Printf.sprintf "verocaml_sel_t%d_%s_%s_i%d_%s_p%s_r%s"
      selector.selector_domain.aggregate_type_index selector.selector_namespace
      (suffix selector.selector_domain)
      selector.selector_index selector.selector_name path range

let named_sort aggregate =
  Printf.sprintf "RankType_%d_%s" aggregate.Vir.aggregate_type_index
    (if aggregate.aggregate_type_arguments = [] then aggregate.aggregate_type_name
     else suffix aggregate)
