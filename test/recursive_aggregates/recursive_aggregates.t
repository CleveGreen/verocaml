The OCaml assertion executable compile-probes the pinned smt.ml 0.25 mapping:
typed aggregate identities use an unconstrained integer carrier, while tag and
selector applications remain namespaced uninterpreted functions in QF_UFLIA.

  $ ./recursive_aggregates_tool.exe unit
  aggregate carrier: typed VIR identity maps to unconstrained SMT Int
  selector/tag encoding: namespaced unary UFs in QF_UFLIA
  selector typing: cross-sort applications rejected before SMT
  Ty_app probe: pinned Z3 mapping rejects unsupported theory app

Compile a real annotated recursive node/stack implementation through the PPX
and consume its CMT. Mutable declarations are admitted, but this feature adds no
field-write semantics.

  $ mkdir artifacts
  $ ocamlc -w -A -alert -all -bin-annot -I ../../runtime/.vero_ghost.objs/byte -ppx "../../ppx/vero_ppx.exe --keep-ghost" -c -o artifacts/recursive_node_stack.cmo fixtures/recursive_node_stack.ml
  $ ./recursive_aggregates_tool.exe dump-sst artifacts/recursive_node_stack.cmt > artifacts/first.sst
  $ ./recursive_aggregates_tool.exe dump-sst artifacts/recursive_node_stack.cmt > artifacts/second.sst
  $ cmp artifacts/first.sst artifacts/second.sst
  $ sed -n '1,14p' artifacts/first.sst
  policy default-linear/default-z3
  type node#0 representation=revealed @ recursive_node_stack.ml:1:0-6:5
    variant
      constructor Empty#0 @ recursive_node_stack.ml:2:2-2:9
      constructor Node#1 @ recursive_node_stack.ml:3:2-6:5
        field constructor-node#0.Node#1.value#0 : int mutability=immutable uniqueness=preserve linearity=preserve @ recursive_node_stack.ml:4:6-4:18
        field constructor-node#0.Node#1.next#1 : node#0 mutability=mutable uniqueness=force-aliased linearity=force-many @ recursive_node_stack.ml:5:6-5:26
  type stack#1 representation=revealed @ recursive_node_stack.ml:8:0-11:1
    record
      field type-stack#1.top#0 : node#0 mutability=mutable uniqueness=force-aliased linearity=force-many @ recursive_node_stack.ml:9:2-9:21
      field type-stack#1.length#1 : int mutability=mutable uniqueness=force-aliased linearity=force-many @ recursive_node_stack.ml:10:2-10:23
  function make_stack#0 mode=exec recursive=false result=stack#1 policy=default-linear/default-z3 @ recursive_node_stack.ml:13:0-16:29
    parameter
      pattern bind n#0:int : int @ recursive_node_stack.ml:13:15-13:16
  $ grep -c '^type node#0 ' artifacts/first.sst
  1
  $ grep -c 'field constructor-node#0.Node#1.next#1 : node#0' artifacts/first.sst
  1

The VIR contains only ground construction equations and one-layer tag/selector
applications. Every exposed symbolic integer selector carries both hardcoded
63-bit range assumptions.

  $ ./recursive_aggregates_tool.exe dump-vir artifacts/recursive_node_stack.cmt > artifacts/first.vir
  $ ./recursive_aggregates_tool.exe dump-vir artifacts/recursive_node_stack.cmt > artifacts/second.vir
  $ cmp artifacts/first.vir artifacts/second.vir
  $ grep -E '^function |\(= \(tag\.node#0 _Node|t0_node_c1_Node_inline\.value#0 \(t0_node_c1_Node\.\$arg0#0|t1_stack_record\.length#1 make_stack\.result' artifacts/first.vir | head -14
  function make_stack#0 mode=exec body=checked-typedtree:recursive_node_stack.ml policy=default-linear/default-z3
  function call_make_stack#1 mode=exec body=checked-typedtree:recursive_node_stack.ml policy=default-linear/default-z3
        (<= -4611686018427387904 (t1_stack_record.length#1 make_stack.result$1))
        (<= (t1_stack_record.length#1 make_stack.result$1) 4611686018427387903)
        (= (t1_stack_record.length#1 make_stack.result$1) n$0)
        (<= -4611686018427387904 (t1_stack_record.length#1 make_stack.result$1))
        (<= (t1_stack_record.length#1 make_stack.result$1) 4611686018427387903)
        (= result$2 (t1_stack_record.length#1 make_stack.result$1))
        (<= -4611686018427387904 (t1_stack_record.length#1 make_stack.result$1))
        (<= (t1_stack_record.length#1 make_stack.result$1) 4611686018427387903)
        (= (t1_stack_record.length#1 make_stack.result$1) n$0)
        (<= -4611686018427387904 (t1_stack_record.length#1 make_stack.result$1))
        (<= (t1_stack_record.length#1 make_stack.result$1) 4611686018427387903)
        (= result$2 (t1_stack_record.length#1 make_stack.result$1))
  $ grep -F '(= (tag.node#0 _Node$3) 1)' artifacts/first.vir | head -1
        (= (tag.node#0 _Node$3) 1)
  $ grep -F '(<= -4611686018427387904 (t0_node_c1_Node_inline.value#0 (t0_node_c1_Node.$arg0#0 param$0)))' artifacts/first.vir | head -1
        (<= -4611686018427387904 (t0_node_c1_Node_inline.value#0 (t0_node_c1_Node.$arg0#0 param$0)))
  $ grep -F '(<= (t0_node_c1_Node_inline.value#0 (t0_node_c1_Node.$arg0#0 param$0)) 4611686018427387903)' artifacts/first.vir | head -1
        (<= (t0_node_c1_Node_inline.value#0 (t0_node_c1_Node.$arg0#0 param$0)) 4611686018427387903)
  $ grep -F '(or (= (tag.node#0 param$0) 0) (= (tag.node#0 param$0) 1))' artifacts/first.vir | head -1
        (or (= (tag.node#0 param$0) 0) (= (tag.node#0 param$0) 1))
  $ grep -F '(or (= (tag.node#0 (t0_node_c1_Node_inline.next#1 (t0_node_c1_Node.$arg0#0 param$0))) 0) (= (tag.node#0 (t0_node_c1_Node_inline.next#1 (t0_node_c1_Node.$arg0#0 param$0))) 1))' artifacts/first.vir | head -1
        (or (= (tag.node#0 (t0_node_c1_Node_inline.next#1 (t0_node_c1_Node.$arg0#0 param$0))) 0) (= (tag.node#0 (t0_node_c1_Node_inline.next#1 (t0_node_c1_Node.$arg0#0 param$0))) 1))

Z3 verifies construction/projection, ordered tag paths, tuple/record results,
and a same-unit summary returning an opaque aggregate.

  $ ./recursive_aggregates_tool.exe solve artifacts/recursive_node_stack.cmt
  make_stack: verified (1 obligations, 1 exits)
  call_make_stack: verified (2 obligations, 1 exits)
  construct_and_match: verified (2 obligations, 2 exits)
  classify_node: verified (0 obligations, 2 exits)
  classify_nested_node: verified (0 obligations, 3 exits)
  record_pattern: verified (0 obligations, 1 exits)
  tuple_with_record: verified (0 obligations, 1 exits)

Known aggregate boundaries are rejected with source locations. Disable color
only for each diagnostic assertion; interactive product styling is unchanged.

  $ compile () { ocamlc -w -A -alert -all -bin-annot -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ for fixture in polymorphic_type imported_abstract cyclic_alias structural_equality partial_match unboxed_variant; do compile "$fixture"; done
  $ OCAML_COLOR=never ./recursive_aggregates_tool.exe classify artifacts/polymorphic_type.cmt
  accepted
  $ OCAML_COLOR=never ./recursive_aggregates_tool.exe classify artifacts/imported_abstract.cmt
  VERO_UNSUPPORTED_TYPE @ imported_abstract.ml:1:20-1:28
  $ OCAML_COLOR=never ./recursive_aggregates_tool.exe classify artifacts/cyclic_alias.cmt
  VERO_UNSUPPORTED_TYPE @ cyclic_alias.ml:2:0-2:16
  $ OCAML_COLOR=never ./recursive_aggregates_tool.exe classify artifacts/structural_equality.cmt
  VERO_UNSUPPORTED_AGGREGATE_EQUALITY @ structural_equality.ml:2:45-2:57
  $ OCAML_COLOR=never ./recursive_aggregates_tool.exe classify artifacts/partial_match.cmt
  VERO_UNSUPPORTED_PARTIAL_MATCH @ partial_match.ml:3:2-3:39
  $ OCAML_COLOR=never ./recursive_aggregates_tool.exe classify artifacts/unboxed_variant.cmt
  VERO_UNSUPPORTED_AGGREGATE @ unboxed_variant.ml:1:0-1:35
