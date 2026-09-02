open Outcome_test_support

let suite_path = "test/broadcast_interfaces/static_elision_cases.ml"

let mismatch message =
  Error (Failure.make Failure.Expectation_mismatch message)

let ends_with value suffix =
  String.length value >= String.length suffix
  && String.equal
       (String.sub value (String.length value - String.length suffix)
          (String.length suffix))
       suffix

let delator_below_error_reference structure =
  let found = ref false in
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.Typedtree.exp_desc with
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                arguments,
                _,
                _,
                _ )
            when ends_with (Path.name path) "Delator.Runtime.event" ->
              let error_level =
                List.exists
                  (function
                    | ( Types.Labelled "level",
                        Typedtree.Arg
                          ( {
                              Typedtree.exp_desc =
                                Texp_construct (_, description, [], None);
                              _;
                            },
                            _ ) ) ->
                        String.equal description.Types.cstr_name "Error"
                    | _ -> false)
                  arguments
              in
              if not error_level then found := true
          | Texp_apply
              ( { exp_desc = Texp_ident (path, _, _, _, _); _ },
                _,
                _,
                _,
                _ )
            when ends_with (Path.name path) "Delator.in_span" ->
              found := true
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  !found

let implementation path =
  match Cmt_format.read path with
  | _, Some { Cmt_format.cmt_annots = Implementation structure; _ } ->
      Ok structure
  | _, Some _ | _, None ->
      mismatch "static-elision evidence is not an implementation CMT"

let paths =
  [
    "../../ppx/.vero_ppx_rewriter.objs/byte/vero_ppx_rewriter__Vero_ppx_broadcast_private.cmt";
    "../../ppx/.vero_ppx_rewriter.objs/byte/vero_ppx_rewriter.cmt";
    "../../src/.verocaml_core.objs/byte/retained_broadcast_private.cmt";
    "../../src/.verocaml_core.objs/byte/retained_interface_authority_private.cmt";
    "../../src/.verocaml_core.objs/byte/cmt_input.cmt";
    "../../src/.verocaml_core.objs/byte/broadcast_declaration_private.cmt";
    "../../src/.verocaml_core.objs/byte/broadcast_scope_private.cmt";
    "../../src/.verocaml_core.objs/byte/broadcast_vc_private.cmt";
    "../../src/.verocaml_core.objs/byte/interface_specification_candidate_private.cmt";
    "../../src/.verocaml_core.objs/byte/interface_specification_environment_private.cmt";
    "../../src/.verocaml_core.objs/byte/interface_specification_loaded_private.cmt";
    "../../src/.verocaml_core.objs/byte/imported_callable.cmt";
    "../../src/.verocaml_core.objs/byte/typedtree_adapter_private.cmt";
    "../../src/.verocaml_core.objs/byte/typedtree_broadcast_private.cmt";
    "../../src/.verocaml_core.objs/byte/typedtree_lowering_private.cmt";
    "../../src/.verocaml_core.objs/byte/verification_scope_private.cmt";
    "../../src/.verocaml_core.objs/byte/external_target_specification_private.cmt";
    "../../src/.verocaml_bin_private.objs/byte/verocaml_bin.cmt";
    "../../src/.verocaml_bin_private.objs/byte/verocaml_bin_dune_private.cmt";
    "../../src/.verocaml_bin_private.objs/byte/verocaml_bin_inventory_private.cmt";
    "../../src/.verocaml_bin_private.objs/byte/verocaml_bin_project_private.cmt";
  ]

let static_elision_case =
  Suite.case ~name:"broadcast-static-error-elides-events-spans-and-fields"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      let rec inspect = function
        | [] ->
            Ok (Outcome.observation ~status:Outcome.Verified () |> Outcome.project)
        | path :: rest -> (
            match implementation path with
            | Error _ as error -> error
            | Ok structure ->
                if delator_below_error_reference structure then
                  mismatch
                    (Printf.sprintf
                       "below-error VERO-116 instrumentation survived in %s"
                       path)
                else inspect rest)
      in
      inspect paths)

let () =
  Suite.run_cli ~suite_path ~manifest:Static_elision_environment.manifest
    ~expected_environment:Static_elision_environment.expected
    [ static_elision_case ]
