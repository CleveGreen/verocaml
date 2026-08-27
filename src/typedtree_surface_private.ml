open Typedtree

type surface = {
  structure_items : int;
  value_bindings : int;
  expression_metadata : int;
  pattern_metadata : int;
  pattern_modes : int;
  parameter_modes : int;
  identifier_unique_uses : int;
  resolved_pattern_barriers : int;
  resolved_field_barriers : int;
  field_reads : int;
  field_writes : int;
  mutable_local_bindings : int;
  mutable_local_reads : int;
  mutable_local_writes : int;
  resolved_calls : int;
  resolved_call_paths : string list;
}

type counters = {
  mutable structure_items : int;
  mutable value_bindings : int;
  mutable expression_metadata : int;
  mutable pattern_metadata : int;
  mutable pattern_modes : int;
  mutable parameter_modes : int;
  mutable identifier_unique_uses : int;
  mutable resolved_pattern_barriers : int;
  mutable resolved_field_barriers : int;
  mutable field_reads : int;
  mutable field_writes : int;
  mutable mutable_local_bindings : int;
  mutable mutable_local_reads : int;
  mutable mutable_local_writes : int;
  mutable resolved_calls : int;
  mutable resolved_call_paths : string list;
}

let empty_counters () =
  {
    structure_items = 0;
    value_bindings = 0;
    expression_metadata = 0;
    pattern_metadata = 0;
    pattern_modes = 0;
    parameter_modes = 0;
    identifier_unique_uses = 0;
    resolved_pattern_barriers = 0;
    resolved_field_barriers = 0;
    field_reads = 0;
    field_writes = 0;
    mutable_local_bindings = 0;
    mutable_local_reads = 0;
    mutable_local_writes = 0;
    resolved_calls = 0;
    resolved_call_paths = [];
  }

(* These typed no-ops deliberately mention the exact pin-bound types.  A
   compiler-libs drift changes this module at compile time rather than leaking
   into later semantic lowering. *)
let observe_location (_ : Location.t) = ()
let observe_type (_ : Types.type_expr) = ()
let observe_environment environment = ignore (Env.summary environment)
let observe_value_mode mode =
  ignore (Format.asprintf "%a" (Mode.Value.print ()) (mode : Mode.Value.l))

let observe_parameter_mode mode =
  ignore (Format.asprintf "%a" (Mode.Alloc.print ()) (mode : Mode.Alloc.l))

let observe_allocation_mode mode =
  ignore (Format.asprintf "%a" (Mode.Alloc.print ()) (mode : Mode.Alloc.r))

let observe_unique_use
    ((uniqueness, linearity) :
      Mode.Uniqueness.r * Mode.Linearity.l) =
  ignore
    (Format.asprintf "%a" (Mode.Uniqueness.print ()) uniqueness);
  ignore (Format.asprintf "%a" (Mode.Linearity.print ()) linearity)

let resolve_barrier barrier =
  match Unique_barrier.resolve barrier with
  | Mode.Uniqueness.Const.Unique -> ()
  | Aliased -> ()

let uniqueness_of_pattern_mode mode =
  match
    Mode.Uniqueness.zap_to_floor
      (Mode.Value.proj_monadic Mode.Axis.Uniqueness mode)
  with
  | Mode.Uniqueness.Const.Unique -> Sst.Definitely_unique
  | Aliased -> Sst.Definitely_aliased

let uniqueness_of_use ((uniqueness, _) : unique_use) =
  match Mode.Uniqueness.zap_to_ceil uniqueness with
  | Mode.Uniqueness.Const.Unique -> Sst.Definitely_unique
  | Aliased -> Sst.Definitely_aliased

let uniqueness_of_parameter_mode mode =
  match (Mode.Alloc.zap_to_floor mode).uniqueness with
  | Mode.Uniqueness.Const.Unique -> Sst.Definitely_unique
  | Aliased -> Sst.Definitely_aliased

let observe_expression_metadata expression =
  let
    {
      exp_desc = _;
      exp_loc;
      exp_extra = _;
      exp_type;
      exp_env;
      exp_attributes = _;
    }
    =
    expression
  in
  observe_location exp_loc;
  observe_type exp_type;
  observe_environment exp_env

let observe_pattern_metadata : type k. k general_pattern -> unit =
 fun pattern ->
  let
    {
      pat_desc = _;
      pat_loc;
      pat_extra = _;
      pat_type;
      pat_env;
      pat_attributes = _;
      pat_unique_barrier;
    }
    =
    pattern
  in
  observe_location pat_loc;
  observe_type pat_type;
  observe_environment pat_env;
  resolve_barrier pat_unique_barrier

let probe structure : surface =
  let counters = empty_counters () in
  let
    {
      str_items = _;
      str_type = (_ : Types.signature);
      str_final_env
    }
    =
    structure
  in
  observe_environment str_final_env;
  let default = Tast_iterator.default_iterator in
  let iterator =
    {
      default with
      structure_item =
        (fun self item ->
          let { str_desc = _; str_loc; str_env } = item in
          counters.structure_items <- counters.structure_items + 1;
          observe_location str_loc;
          observe_environment str_env;
          default.structure_item self item);
      value_binding =
        (fun self binding ->
          let
            {
              vb_pat = _;
              vb_expr = _;
              vb_rec_kind = _;
              vb_sort = _;
              vb_attributes = _;
              vb_loc;
            }
            =
            binding
          in
          counters.value_bindings <- counters.value_bindings + 1;
          observe_location vb_loc;
          default.value_binding self binding);
      pat =
        (fun (type k) self (pattern : k general_pattern) ->
          counters.pattern_metadata <- counters.pattern_metadata + 1;
          observe_pattern_metadata pattern;
          counters.resolved_pattern_barriers <-
            counters.resolved_pattern_barriers + 1;
          (match pattern.pat_desc with
          | Tpat_var (_, _, _, _, mode) ->
              counters.pattern_modes <- counters.pattern_modes + 1;
              observe_value_mode mode
          | Tpat_alias (_, _, _, _, _, mode, _) ->
              counters.pattern_modes <- counters.pattern_modes + 1;
              observe_value_mode mode
          | _ -> ());
          default.pat self pattern);
      expr =
        (fun self expression ->
          counters.expression_metadata <- counters.expression_metadata + 1;
          observe_expression_metadata expression;
          (match expression.exp_desc with
          | Texp_ident
              ( path,
                (_ : Longident.t Location.loc),
                (_ : Types.value_description),
                (_ : ident_kind),
                unique_use ) ->
              ignore (Path.head path);
              observe_unique_use unique_use;
              counters.identifier_unique_uses <-
                counters.identifier_unique_uses + 1
          | Texp_function
              {
                params;
                body = _;
                ret_mode;
                ret_sort = _;
                alloc_mode;
                zero_alloc = _;
              } ->
              observe_parameter_mode ret_mode;
              observe_allocation_mode alloc_mode;
              List.iter
                (fun
                  {
                    fp_arg_label = _;
                    fp_param = _;
                    fp_param_debug_uid = _;
                    fp_partial = _;
                    fp_kind = _;
                    fp_sort = _;
                    fp_mode;
                    fp_curry = _;
                    fp_newtypes = _;
                    fp_loc;
                  }
                ->
                  counters.parameter_modes <- counters.parameter_modes + 1;
                  observe_parameter_mode fp_mode;
                  observe_location fp_loc)
                params
          | Texp_field
              ( _,
                _,
                (_ : Longident.t Location.loc),
                (_ : Types.label_description),
                (_ : texp_field_boxing),
                barrier ) ->
              resolve_barrier barrier;
              counters.field_reads <- counters.field_reads + 1;
              counters.resolved_field_barriers <-
                counters.resolved_field_barriers + 1
          | Texp_setfield
              ( _,
                (_ : Mode.Locality.l),
                (_ : Longident.t Location.loc),
                (_ : Types.label_description),
                _ ) ->
              counters.field_writes <- counters.field_writes + 1
          | Texp_letmutable _ ->
              counters.mutable_local_bindings <-
                counters.mutable_local_bindings + 1
          | Texp_mutvar (_ : Ident.t Location.loc) ->
              counters.mutable_local_reads <- counters.mutable_local_reads + 1
          | Texp_setmutvar
              ( (_ : Ident.t Location.loc),
                (_ : Jkind.sort),
                _ ) ->
              counters.mutable_local_writes <-
                counters.mutable_local_writes + 1
          | Texp_apply
              ( callee,
                (_ : (arg_label * apply_arg) list),
                (_ : apply_position),
                (_ : Mode.Locality.l),
                (_ : Zero_alloc.assume option) ) -> (
              match callee.exp_desc with
              | Texp_ident (path, _, _, _, _) ->
                  counters.resolved_calls <- counters.resolved_calls + 1;
                  counters.resolved_call_paths <-
                    Path.name path :: counters.resolved_call_paths
              | _ -> ())
          | _ -> ());
          default.expr self expression);
    }
  in
  iterator.structure iterator structure;
  ({
    structure_items = counters.structure_items;
    value_bindings = counters.value_bindings;
    expression_metadata = counters.expression_metadata;
    pattern_metadata = counters.pattern_metadata;
    pattern_modes = counters.pattern_modes;
    parameter_modes = counters.parameter_modes;
    identifier_unique_uses = counters.identifier_unique_uses;
    resolved_pattern_barriers = counters.resolved_pattern_barriers;
    resolved_field_barriers = counters.resolved_field_barriers;
    field_reads = counters.field_reads;
    field_writes = counters.field_writes;
    mutable_local_bindings = counters.mutable_local_bindings;
    mutable_local_reads = counters.mutable_local_reads;
    mutable_local_writes = counters.mutable_local_writes;
    resolved_calls = counters.resolved_calls;
    resolved_call_paths = List.rev counters.resolved_call_paths;
  } : surface)
