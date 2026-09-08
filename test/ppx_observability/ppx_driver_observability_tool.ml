let contains_substring text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec search offset =
    offset + substring_length <= text_length
    &&
    (String.equal (String.sub text offset substring_length) substring
    || search (offset + 1))
  in
  search 0

let reject_delator_reference rendered =
  if contains_substring rendered "Delator" then
    failwith "the public PPX introduced a Delator reference into user code"

let implementation () =
  Parse.implementation (Lexing.from_string "let identity value = value")
  |> Vero_ppx_driver.implementation
  |> Format.asprintf "%a" Pprintast.structure
  |> reject_delator_reference

let interface () =
  Parse.interface (Lexing.from_string "val identity : int -> int")
  |> Vero_ppx_driver.interface
  |> Format.asprintf "%a" Pprintast.signature
  |> reject_delator_reference

type route = Standalone | Ppxlib
type mode = Ordinary | Retained
type entrypoint = Implementation | Interface

let parse_route = function
  | "standalone" -> Standalone
  | "ppxlib" -> Ppxlib
  | value -> invalid_arg ("unknown PPX route: " ^ value)

let parse_mode = function
  | "ordinary" -> Ordinary
  | "retained" -> Retained
  | value -> invalid_arg ("unknown PPX mode: " ^ value)

let parse_entrypoint = function
  | "implementation" -> Implementation
  | "interface" -> Interface
  | value -> invalid_arg ("unknown PPX entrypoint: " ^ value)

let standalone_arguments = function
  | Ordinary -> []
  | Retained -> [ "--keep-ghost" ]

let transform_structure route mode structure =
  match route with
  | Standalone ->
      let mapper =
        Vero_ppx_rewriter.make ~entrypoint:Vero_ppx_rewriter.Implementation
          (standalone_arguments mode)
      in
      mapper.Ast_mapper.structure mapper structure
  | Ppxlib ->
      Vero_ppx_driver.select_mode
        (match mode with
        | Ordinary -> Vero_ppx_driver.Ordinary
        | Retained -> Vero_ppx_driver.Retained)
        ();
      Vero_ppx_driver.implementation structure

let transform_signature route mode signature =
  match route with
  | Standalone ->
      let mapper =
        Vero_ppx_rewriter.make ~entrypoint:Vero_ppx_rewriter.Interface
          (standalone_arguments mode)
      in
      mapper.Ast_mapper.signature mapper signature
  | Ppxlib ->
      Vero_ppx_driver.select_mode
        (match mode with
        | Ordinary -> Vero_ppx_driver.Ordinary
        | Retained -> Vero_ppx_driver.Retained)
        ();
      Vero_ppx_driver.interface signature

let value_names signature =
  List.filter_map
    (fun item ->
      match item.Parsetree.psig_desc with
      | Psig_value value -> Some value.pval_name.txt
      | _ -> None)
    signature.Parsetree.psg_items
  |> List.sort String.compare

let binding_names structure =
  List.concat_map
    (fun item ->
      match item.Parsetree.pstr_desc with
      | Pstr_value (_, bindings) ->
          List.filter_map
            (fun (binding : Parsetree.value_binding) ->
              match binding.pvb_pat.ppat_desc with
              | Ppat_var name -> Some name.txt
              | _ -> None)
            bindings
      | _ -> [])
    structure
  |> List.sort String.compare

let logical_constant_markers structure =
  structure
  |> List.concat_map (fun item ->
         match item.Parsetree.pstr_desc with
         | Pstr_value (_, bindings) ->
             bindings
             |> List.concat_map (fun binding ->
                    binding.Parsetree.pvb_attributes
                    |> List.filter (fun (attribute : Parsetree.attribute) ->
                           String.equal attribute.attr_name.txt
                             "verocaml.internal.logical_constant.definition.v1"))
         | _ -> [])

let module_structure expression =
  let rec find expression =
    match expression.Parsetree.pmod_desc with
    | Pmod_structure structure -> structure
    | Pmod_constraint (expression, _, _) -> find expression
    | _ -> failwith "semantic fixture module has no structure body"
  in
  find expression

let require_names label expected actual =
  let expected = List.sort String.compare expected in
  if expected <> actual then
    failwith
      (Printf.sprintf "%s names differ: expected [%s], got [%s]" label
         (String.concat "," expected) (String.concat "," actual))

let implementation_source =
  {|
module type SERVICE = sig
  val execute : int -> int
  val answer : int
  val model : int -> int
  val invariant : int -> bool
  val lemma : int -> unit
end

module Service : SERVICE = struct
  let execute value = value
  let answer : int = 42 [@@verocaml.spec]
  let model value = value [@@verocaml.spec]
  let invariant value = value >= 0 [@@verocaml.type_invariant]
  let lemma _value = () [@@verocaml.proof]
end
|}

let interface_source =
  {|
val execute : int -> int
val model : int -> int [@@verocaml.spec]
val invariant : int -> bool [@@verocaml.type_invariant]
val lemma : int -> unit [@@verocaml.proof]

module Nested : sig
  val execute : int -> int
  val model : int -> int [@@verocaml.spec]
end
|}

let semantic_implementation route mode =
  let structure =
    Parse.implementation (Lexing.from_string implementation_source)
    |> transform_structure route mode
  in
  let module_type_names, module_names, constant_marker_count =
    List.fold_left
      (fun (module_type_names, module_names, constant_marker_count) item ->
        match item.Parsetree.pstr_desc with
        | Pstr_modtype
            {
              pmtd_name = { txt = "SERVICE"; _ };
              pmtd_type = Some { pmty_desc = Pmty_signature signature; _ };
              _;
            } ->
            (value_names signature, module_names, constant_marker_count)
        | Pstr_module
            {
              pmb_name = { txt = Some "Service"; _ };
              pmb_expr;
              _;
            } ->
            let body = module_structure pmb_expr in
            ( module_type_names,
              binding_names body,
              List.length (logical_constant_markers body) )
        | _ -> (module_type_names, module_names, constant_marker_count))
      ([], [], 0) structure
  in
  let expected =
    match mode with
    | Ordinary -> [ "execute" ]
    | Retained -> [ "answer"; "execute"; "invariant"; "lemma"; "model" ]
  in
  require_names "module type" expected module_type_names;
  require_names "module implementation" expected module_names;
  let expected_marker_count = match mode with Ordinary -> 0 | Retained -> 1 in
  if constant_marker_count <> expected_marker_count then
    failwith
      (Printf.sprintf
         "logical constant marker count differs: expected %d, got %d"
         expected_marker_count constant_marker_count)

let nested_signature_names signature =
  List.find_map
    (fun item ->
      match item.Parsetree.psig_desc with
      | Psig_module
          {
            pmd_name = { txt = Some "Nested"; _ };
            pmd_type = { pmty_desc = Pmty_signature nested; _ };
            _;
          } ->
          Some (value_names nested)
      | _ -> None)
    signature
  |> Option.value ~default:[]

let semantic_interface route mode =
  let signature =
    Parse.interface (Lexing.from_string interface_source)
    |> transform_signature route mode
  in
  let expected_root, expected_nested =
    match mode with
    | Ordinary -> ([ "execute" ], [ "execute" ])
    | Retained ->
        ([ "execute"; "invariant"; "lemma"; "model" ],
         [ "execute"; "model" ])
  in
  require_names "interface" expected_root (value_names signature);
  require_names "nested interface" expected_nested
    (nested_signature_names signature.Parsetree.psg_items)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] ->
      implementation ();
      interface ()
  | [ _; "--semantic"; route; mode; entrypoint ] -> (
      match parse_entrypoint entrypoint with
      | Implementation ->
          semantic_implementation (parse_route route) (parse_mode mode)
      | Interface -> semantic_interface (parse_route route) (parse_mode mode))
  | _ ->
      invalid_arg
        "usage: ppx_driver_observability_tool [--semantic ROUTE MODE ENTRYPOINT]"
