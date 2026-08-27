open Ast_helper
open Asttypes
open Parsetree

type kind = Call_requires | Call_ensures | Forall | Exists

let kind_name = function
  | Call_requires -> "call_requires"
  | Call_ensures -> "call_ensures"
  | Forall -> "forall"
  | Exists -> "exists"

let kind_of_name = function
  | "call_requires" -> Some Call_requires
  | "call_ensures" -> Some Call_ensures
  | "forall" -> Some Forall
  | "exists" -> Some Exists
  | _ -> None

type logical_scope = {
  mutable depth : int;
  mutable ensures_depth : int;
}

let create_logical_scope () = { depth = 0; ensures_depth = 0 }

let with_logical_payload scope action =
  scope.depth <- scope.depth + 1;
  Fun.protect
    ~finally:(fun () -> scope.depth <- scope.depth - 1)
    action

let with_ensures scope action =
  scope.ensures_depth <- scope.ensures_depth + 1;
  Fun.protect
    ~finally:(fun () -> scope.ensures_depth <- scope.ensures_depth - 1)
    action

let inside_ensures scope = scope.ensures_depth <> 0

let map_declaration_body scope ~role ~map expression =
  if
    List.mem role [ "spec"; "proof"; "type-invariant"; "recursive-spec" ]
  then with_logical_payload scope (fun () -> map expression)
  else map expression

let require_function_body ~attribute expression =
  match expression.Parsetree.pexp_desc with
  | Pexp_function (_, _, Pfunction_body _) -> ()
  | _ ->
      Location.raise_errorf ~loc:expression.pexp_loc
        "[@@%s] requires a function with an expression body" attribute

let rec pattern_value_bindings pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> [ name ]
  | Ppat_alias (pattern, name) -> pattern_value_bindings pattern @ [ name ]
  | Ppat_tuple (components, _)
  | Ppat_unboxed_tuple (components, _) ->
      List.concat_map (fun (_, pattern) -> pattern_value_bindings pattern) components
  | Ppat_construct (_, Some (_, pattern))
  | Ppat_variant (_, Some pattern)
  | Ppat_constraint (pattern, _, _)
  | Ppat_lazy pattern
  | Ppat_exception pattern
  | Ppat_open (_, pattern) ->
      pattern_value_bindings pattern
  | Ppat_record (fields, _)
  | Ppat_record_unboxed_product (fields, _) ->
      List.concat_map (fun (_, pattern) -> pattern_value_bindings pattern) fields
  | Ppat_array (_, patterns) ->
      List.concat_map pattern_value_bindings patterns
  | Ppat_or (left, right) ->
      pattern_value_bindings left @ pattern_value_bindings right
  | Ppat_any | Ppat_constant _ | Ppat_interval _ | Ppat_construct (_, None)
  | Ppat_variant (_, None) | Ppat_type _ | Ppat_unpack _ | Ppat_extension _ ->
      []

let add_visible_binding visible name =
  List.filter (fun existing -> not (String.equal existing.txt name.txt)) visible
  @ [ name ]

let visible_parameter_bindings parameters =
  List.fold_left
    (fun visible parameter ->
      match parameter.pparam_desc with
      | Pparam_val (_, _, pattern) ->
          List.fold_left add_visible_binding visible
            (pattern_value_bindings pattern)
      | Pparam_newtype _ -> visible)
    [] parameters

let aliased_shadow name =
  let loc = { name.loc with loc_ghost = true } in
  let mode = { txt = Mode "aliased"; loc } in
  let pattern =
    Pat.constraint_ ~loc (Pat.var ~loc { name with loc })
      (Some (Typ.any ~loc None)) [ mode ]
  in
  {
    pparam_loc = loc;
    pparam_desc = Pparam_val (Nolabel, None, pattern);
  }

let mentioned_identifiers expression =
  let names = ref [] in
  let default = Ast_iterator.default_iterator in
  let iterator =
    {
      default with
      expr =
        (fun self expression ->
          (match expression.pexp_desc with
          | Pexp_ident { txt = Longident.Lident name; _ } ->
              names := name :: !names
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.expr iterator expression;
  List.sort_uniq String.compare !names


let local_binding_name binding =
  match binding.Parsetree.pvb_pat.ppat_desc with
  | Ppat_var name -> Some name
  | Ppat_any | Ppat_alias _ | Ppat_constant _ | Ppat_interval _
  | Ppat_tuple _ | Ppat_unboxed_tuple _ | Ppat_construct _ | Ppat_variant _
  | Ppat_record _ | Ppat_record_unboxed_product _ | Ppat_array _ | Ppat_or _
  | Ppat_constraint _ | Ppat_type _ | Ppat_lazy _ | Ppat_unpack _
  | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
      None

let map_let ~reject_attributes ~map_binding ~map_expression ~lexical_bindings
    expression rec_flag value_constraint bindings body =
  List.iter
    (fun binding -> reject_attributes binding.Parsetree.pvb_attributes)
    bindings;
  let mapped_bindings = List.map map_binding bindings in
  let local_bindings = List.filter_map local_binding_name bindings in
  let saved = !lexical_bindings in
  lexical_bindings := local_bindings @ saved;
  let mapped_body =
    Fun.protect
      ~finally:(fun () -> lexical_bindings := saved)
      (fun () -> map_expression body)
  in
  {
    expression with
    pexp_desc =
      Pexp_let (rec_flag, value_constraint, mapped_bindings, mapped_body);
  }

let map_function ~visible_parameters ~lexical_bindings ~function_scopes ~map
    expression parameters body =
  let names =
    visible_parameters parameters @ !lexical_bindings
    @
    match !function_scopes with
    | (_, names) :: _ -> names
    | [] -> []
  in
  let saved = !function_scopes in
  function_scopes := (body, names) :: saved;
  Fun.protect
    ~finally:(fun () -> function_scopes := saved)
    (fun () -> map expression)

let rewrite_scoped_body ~function_scopes ~lexical_bindings ~rewrite expression =
  match !function_scopes with
  | (body, parameters) :: outer when body == expression ->
      let saved = !function_scopes in
      let saved_bindings = !lexical_bindings in
      function_scopes := outer;
      lexical_bindings := parameters @ saved_bindings;
      Some
        (Fun.protect
           ~finally:(fun () ->
             function_scopes := saved;
             lexical_bindings := saved_bindings)
           (fun () -> rewrite parameters expression))
  | _ -> None

let unqualified_kind expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Longident.Lident name; _ } -> kind_of_name name
  | _ -> None

let reserved expression =
  match expression.pexp_desc with
  | Pexp_ident _ -> Option.is_some (unqualified_kind expression)
  | Pexp_apply (callee, _) -> Option.is_some (unqualified_kind callee)
  | _ -> false

let ghost loc = { loc with Location.loc_ghost = true }

let bool ~loc value =
  Exp.construct ~loc
    { txt = Longident.Lident (if value then "true" else "false"); loc }
    None

let marker ~kind source_loc =
  let loc = ghost source_loc in
  let value =
    Printf.sprintf "verocaml:logical-builtin:1:%s:%d:%d"
      (kind_name kind) source_loc.loc_start.pos_cnum source_loc.loc_end.pos_cnum
  in
  Exp.apply ~loc
    (Exp.ident ~loc
       {
         txt = Longident.Ldot (Longident.Lident "Vero_ghost", "marker");
         loc;
       })
    [
      ( Nolabel,
        Exp.constant ~loc
          (Pconst_string (value, loc, None)) );
    ]

let constrained_any ~loc name =
  Pat.constraint_ ~loc (Pat.any ~loc ())
    (Some (Typ.var ~loc name None)) []

let helper ~kind source_loc =
  let loc = ghost source_loc in
  let result = bool ~loc true in
  let parameter pattern body =
    {
      pparam_loc = loc;
      pparam_desc = Pparam_val (Nolabel, None, pattern);
    },
    body
  in
  let constraint_ =
    {
      mode_annotations = [];
      ret_mode_annotations = [];
      ret_type_constraint = None;
    }
  in
  match kind with
  | Call_requires ->
      let parameter, body = parameter (Pat.any ~loc ()) result in
      Exp.function_ ~loc [ parameter ] constraint_ (Pfunction_body body)
  | Call_ensures ->
      let variable =
        Printf.sprintf "vero103_result_%d" source_loc.loc_start.pos_cnum
      in
      let first, body =
        parameter (constrained_any ~loc variable) result
      in
      let second, body =
        parameter (constrained_any ~loc variable) body
      in
      Exp.function_ ~loc [ first; second ] constraint_ (Pfunction_body body)
  | Forall | Exists ->
      let variable =
        Printf.sprintf "vero103_binder_%d" source_loc.loc_start.pos_cnum
      in
      let predicate_type =
        Typ.arrow ~loc Nolabel (Typ.var ~loc variable None)
          (Typ.constr ~loc { txt = Longident.Lident "bool"; loc } [])
          [] []
      in
      let parameter, body =
        parameter
          (Pat.constraint_ ~loc (Pat.any ~loc ())
             (Some predicate_type) [])
          result
      in
      Exp.function_ ~loc [ parameter ] constraint_ (Pfunction_body body)

let expected_arity = function
  | Call_requires | Forall | Exists -> 1
  | Call_ensures -> 2

let malformed ~loc kind =
  Location.raise_errorf ~loc
    "reserved logical builtin %s expects exactly %d unlabelled argument(s)"
    (kind_name kind) (expected_arity kind)

let rewrite ~map expression =
  match expression.pexp_desc with
  | Pexp_apply (callee, arguments) -> (
      match unqualified_kind callee with
      | None -> None
      | Some kind ->
          if
            List.length arguments <> expected_arity kind
            || not
                 (List.for_all
                    (fun (label, _) -> label = Nolabel)
                    arguments)
          then malformed ~loc:expression.pexp_loc kind;
          let arguments =
            List.map (fun (label, argument) -> (label, map argument)) arguments
          in
          let loc = ghost expression.pexp_loc in
          let application =
            Exp.apply ~loc (helper ~kind expression.pexp_loc) arguments
          in
          Some
            (Exp.sequence ~loc
               (marker ~kind expression.pexp_loc)
               application))
  | Pexp_ident _ -> (
      match unqualified_kind expression with
      | Some kind -> malformed ~loc:expression.pexp_loc kind
      | None -> None)
  | _ -> None

let rewrite_in_payload scope ~map expression =
  if scope.depth = 0 then None else rewrite ~map expression
