Compile the focused corpus with the pinned compiler. Rank certification consumes
the retained Typedtree identities; no source spelling or raw SST flag can issue
a domain.

  $ mkdir artifacts
  $ compile () { ocamlc -w -A -alert -all -bin-annot -c -o "artifacts/$1.cmo" "fixtures/$1.ml"; }
  $ for fixture in positive direct_negative alias_negative wrapper_negative groundless record_cycle mutable_child generic_positive gadt object_proxy external_proxy generic_profiles_positive generic_profile_classes generic_closed_dependent generic_negative_launder generic_dependent_open generic_arrow_groundless generic_external generic_abstract generic_open generic_mutable generic_gadt generic_higher_kinded generic_mutual generic_hidden_opaque profile_same_name_a profile_same_name_b; do compile "$fixture"; done

Local monomorphic immutable list/tree variants receive two exact-snapshot
domains. Compiler paths, declaration/constructor identities, ground witnesses,
positive child positions, and complete expansion traces are deterministic.

  $ ./recursive_rank_tool.exe domains artifacts/positive.cmt > artifacts/domains-1
  $ ./recursive_rank_tool.exe domains artifacts/positive.cmt > artifacts/domains-2
  $ cmp artifacts/domains-1 artifacts/domains-2
  $ sed -E 's/[0-9a-f]{32}/<digest>/g' artifacts/domains-1
  rank-domains=2
  domain id=rank-domain/v1/<digest> version=structural-height-v1 digest=<digest> immutable=true
    component int_list#0 path=int_list uid=Positive.0
    ground int_list#0.Nil#0 uid=Positive.1 rank=0
    child int_list#0.Cons#1 field=$1#1 path=root field-uid=tuple:Positive.2:1 constructor-uid=Positive.2 target=int_list#0
      trace type:int_list[path=int_list,uid=Positive.0] -> constructor:Cons[uid=Positive.2] -> field:$1[uid=tuple:Positive.2:1] -> apply:type:int_list[path=int_list,uid=Positive.0]
  domain id=rank-domain/v1/<digest> version=structural-height-v1 digest=<digest> immutable=true
    component tree#1 path=tree uid=Positive.3
    ground tree#1.Leaf#0 uid=Positive.4 rank=0
    child tree#1.Branch#1 field=$0#0 path=root field-uid=tuple:Positive.5:0 constructor-uid=Positive.5 target=tree#1
      trace type:tree[path=tree,uid=Positive.3] -> constructor:Branch[uid=Positive.5] -> field:$0[uid=tuple:Positive.5:0] -> apply:type:tree[path=tree,uid=Positive.3]
    child tree#1.Branch#1 field=$2#2 path=root field-uid=tuple:Positive.5:2 constructor-uid=Positive.5 target=tree#1
      trace type:tree[path=tree,uid=Positive.3] -> constructor:Branch[uid=Positive.5] -> field:$2[uid=tuple:Positive.5:2] -> apply:type:tree[path=tree,uid=Positive.3]

VIR exposes only domain metadata, finite ground bases, constructor
nonnegativity, and one-layer positive-child-smaller facts. It emits no
termination/decreases obligation.

  $ ./recursive_rank_tool.exe vir artifacts/positive.cmt > artifacts/rank-1.vir
  $ ./recursive_rank_tool.exe vir artifacts/positive.cmt > artifacts/rank-2.vir
  $ cmp artifacts/rank-1.vir artifacts/rank-2.vir
  $ sed -E 's/[0-9a-f]{32}/<digest>/g' artifacts/rank-1.vir
  policy default-linear/default-z3
  rank-domain id=rank-domain/v1/<digest> version=structural-height-v1 digest=<digest> immutable=true
    component int_list#0
    ground-base constructor=int_list#0.Nil#0 rank=0
    positive-child-smaller constructor=int_list#0.Cons#1 field=$1#1 child=int_list#0
    constructor-nonnegative constructor=int_list#0.Nil#0
    constructor-nonnegative constructor=int_list#0.Cons#1
  rank-domain id=rank-domain/v1/<digest> version=structural-height-v1 digest=<digest> immutable=true
    component tree#1
    ground-base constructor=tree#1.Leaf#0 rank=0
    positive-child-smaller constructor=tree#1.Branch#1 field=$0#0 child=tree#1
    positive-child-smaller constructor=tree#1.Branch#1 field=$2#2 child=tree#1
    constructor-nonnegative constructor=tree#1.Leaf#0
    constructor-nonnegative constructor=tree#1.Branch#1
  $ grep -Ec 'entry-measure|recursive-call.*descent' artifacts/rank-1.vir || :
  0

The domain-labelled Logic IR renders deterministic AUFLIA declarations,
explicit child/nonnegative patterns, and finite base assertions. It performs no
public logical datatype expansion.

  $ ./recursive_rank_tool.exe smt artifacts/positive.cmt > artifacts/rank-1.smt
  $ ./recursive_rank_tool.exe smt artifacts/positive.cmt > artifacts/rank-2.smt
  $ cmp artifacts/rank-1.smt artifacts/rank-2.smt
  $ grep -c '^domain-query ' artifacts/rank-1.smt
  2
  $ grep -c '^sort:' artifacts/rank-1.smt
  2
  $ grep -c '^fun:.*rank_d' artifacts/rank-1.smt
  2
  $ grep -c ':qid verocaml.rank.*child)' artifacts/rank-1.smt
  3
  $ grep -c ':qid verocaml.rank.*nonnegative)' artifacts/rank-1.smt
  4
  $ grep -c '^(assert (= (rank_.*rank_ground.*) 0))' artifacts/rank-1.smt
  2
  $ grep -c '^render-counters contexts=1 solvers=1 cleaned=1 live=0$' artifacts/rank-1.smt
  2

Direct and transitive function-domain occurrences report the complete
compiler-identity expansion chain. Groundless and record/wrapper-only cycles
fail before either solver is created.

  $ OCAML_COLOR=never ./recursive_rank_tool.exe classify artifacts/direct_negative.cmt
  VERO_INVALID_RECURSIVE_RANK: negative recursive occurrence in type:t[path=t,uid=Direct_negative.0]; expansion trace: type:t[path=t,uid=Direct_negative.0] -> constructor:K[uid=Direct_negative.1]>field:$0[uid=tuple:Direct_negative.1:0] -> arrow-domain -> apply:type:t[path=t,uid=Direct_negative.0] @ direct_negative.ml:1:0-1:24
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  $ OCAML_COLOR=never ./recursive_rank_tool.exe classify artifacts/alias_negative.cmt
  VERO_INVALID_RECURSIVE_RANK: negative recursive occurrence in type:t[path=t,uid=Alias_negative.0]; expansion trace: type:t[path=t,uid=Alias_negative.0] -> constructor:K[uid=Alias_negative.2]>field:$0[uid=tuple:Alias_negative.2:0] -> apply:type:negative[path=negative,uid=Alias_negative.1] -> expand:type:negative[path=negative,uid=Alias_negative.1] -> manifest -> arrow-domain -> apply:type:t[path=t,uid=Alias_negative.0] @ alias_negative.ml:1:0-1:22
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  $ OCAML_COLOR=never ./recursive_rank_tool.exe classify artifacts/wrapper_negative.cmt
  VERO_INVALID_RECURSIVE_RANK: generic rank application uses a prohibited negative parameter in type:t[path=t,uid=Wrapper_negative.2]; expansion trace: type:t[path=t,uid=Wrapper_negative.2] -> constructor:K[uid=Wrapper_negative.4]>field:$0[uid=tuple:Wrapper_negative.4:0] -> apply:type:wrapper[path=wrapper,uid=Wrapper_negative.0] -> substitute:parameter#0=type:t[path=t,uid=Wrapper_negative.2]<> -> expand:type:wrapper[path=wrapper,uid=Wrapper_negative.0] -> constructor:W[uid=Wrapper_negative.1]>field:$0[uid=tuple:Wrapper_negative.1:0] -> arrow-domain -> apply:type:t[path=t,uid=Wrapper_negative.2] @ wrapper_negative.ml:2:0-2:32
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  $ OCAML_COLOR=never ./recursive_rank_tool.exe classify artifacts/groundless.cmt
  VERO_INVALID_RECURSIVE_RANK: recursive rank component has no finite ground constructor: type:t[path=t,uid=Groundless.0] @ groundless.ml:1:0-1:15
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  $ OCAML_COLOR=never ./recursive_rank_tool.exe classify artifacts/record_cycle.cmt
  VERO_INVALID_RECURSIVE_RANK: recursive rank component contains a record-only construction cycle at type:wrapper[path=wrapper,uid=Record_cycle.1] @ record_cycle.ml:2:0-2:26
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  $ OCAML_COLOR=never ./recursive_rank_tool.exe classify artifacts/external_proxy.cmt
  VERO_INVALID_RECURSIVE_RANK: recursive rank occurrence crosses an unsupported external expansion in type:t[path=t,uid=External_proxy.0]; expansion trace: type:t[path=t,uid=External_proxy.0] -> constructor:Node[uid=External_proxy.2]>field:$0[uid=tuple:External_proxy.2:0] -> external-application:option -> apply:type:t[path=t,uid=External_proxy.0] @ external_proxy.ml:1:0-1:34
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

The monomorphic slice rejects generic/GADT/object forms through the existing
frontend boundary, while mutable recursive aggregates preserve their prior
behavior but receive zero rank domains.

  $ for fixture in mutable_child generic_positive gadt object_proxy; do OCAML_COLOR=never ./recursive_rank_tool.exe classify "artifacts/$fixture.cmt"; done
  accepted rank-domains=0
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  accepted rank-domains=0
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_UNSUPPORTED_POLYMORPHISM: polymorphic functions are not supported @ gadt.ml:2:2-2:14
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_UNSUPPORTED_TYPE: This type is not supported in verified code. @ object_proxy.ml:3:13-3:26
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

Generic rank certification remains private to Typedtree identities. Immutable
list, tree, container, alias, and resolved applications produce deterministic
profiles without extending SST values or reaching VIR.

  $ ./recursive_rank_tool.exe profiles artifacts/generic_profiles_positive.cmt > artifacts/profiles-1
  $ ./recursive_rank_tool.exe profiles artifacts/generic_profiles_positive.cmt > artifacts/profiles-2
  $ cmp artifacts/profiles-1 artifacts/profiles-2
  $ sed -E 's/[0-9a-f]{32}/<digest>/g; s/open:a#[0-9]+/open:a#<id>/g' artifacts/profiles-1 | grep -E '^(rank-profiles=|profile id=|  parameter |  application target=container_alias|    substitution parameter#0=wrapped|solver-counters)'
  rank-profiles=5
  profile id=rank-profile/v1/rank_list/Generic_profiles_positive.0/<digest> digest=<digest> constructor=rank_list#0 path=rank_list uid=Generic_profiles_positive.0 independent=true
    parameter 0 identity=parameter:a#0 class=independently-grounded variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
  profile id=rank-profile/v1/rank_tree/Generic_profiles_positive.3/<digest> digest=<digest> constructor=rank_tree#1 path=rank_tree uid=Generic_profiles_positive.3 independent=true
    parameter 0 identity=parameter:a#0 class=independently-grounded variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
  profile id=rank-profile/v1/container/Generic_profiles_positive.6/<digest> digest=<digest> constructor=container#2 path=container uid=Generic_profiles_positive.6 independent=true
    parameter 0 identity=parameter:a#0 class=independently-grounded variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
  profile id=rank-profile/v1/container_alias/Generic_profiles_positive.9/<digest> digest=<digest> constructor=container_alias#3 path=container_alias uid=Generic_profiles_positive.9 independent=true
    parameter 0 identity=parameter:a#0 class=independently-grounded variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
  profile id=rank-profile/v1/wrapped/Generic_profiles_positive.10/<digest> digest=<digest> constructor=wrapped#4 path=wrapped uid=Generic_profiles_positive.10 independent=true
      substitution parameter#0=wrapped[path=wrapped,uid=Generic_profiles_positive.10]<>
    application target=container_alias#3 path=container_alias uid=Generic_profiles_positive.9 profile=rank-profile/v1/container_alias/Generic_profiles_positive.9/<digest>
      substitution parameter#0=wrapped[path=wrapped,uid=Generic_profiles_positive.10]<>
    application target=container_alias#3 path=container_alias uid=Generic_profiles_positive.9 profile=rank-profile/v1/container_alias/Generic_profiles_positive.9/<digest>
      substitution parameter#0=wrapped[path=wrapped,uid=Generic_profiles_positive.10]<>
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  $ grep '^  grounding' artifacts/profiles-1
    grounding type:rank_list[path=rank_list,uid=Generic_profiles_positive.0] -> constructor:Nil[uid=Generic_profiles_positive.1]
    grounding type:rank_tree[path=rank_tree,uid=Generic_profiles_positive.3] -> constructor:Leaf[uid=Generic_profiles_positive.4]
    grounding type:container[path=container,uid=Generic_profiles_positive.6] -> constructor:Empty[uid=Generic_profiles_positive.7]
    grounding type:container_alias[path=container_alias,uid=Generic_profiles_positive.9] -> manifest -> apply:type:container[path=container,uid=Generic_profiles_positive.6] -> substitute:parameter#0=parameter:a#0 -> expand:type:container[path=container,uid=Generic_profiles_positive.6] -> constructor:Empty[uid=Generic_profiles_positive.7]
    grounding type:wrapped[path=wrapped,uid=Generic_profiles_positive.10] -> constructor:Wrap[uid=Generic_profiles_positive.11] -> apply:type:container_alias[path=container_alias,uid=Generic_profiles_positive.9] -> substitute:parameter#0=type:wrapped[path=wrapped,uid=Generic_profiles_positive.10]<> -> expand:type:container_alias[path=container_alias,uid=Generic_profiles_positive.9] -> manifest -> apply:type:container[path=container,uid=Generic_profiles_positive.6] -> substitute:parameter#0=type:wrapped[path=wrapped,uid=Generic_profiles_positive.10]<> -> expand:type:container[path=container,uid=Generic_profiles_positive.6] -> constructor:Empty[uid=Generic_profiles_positive.7]

All three sealed parameter classifications are derived structurally. Compiler
variance is printed only as a consistency fact.

  $ ./recursive_rank_tool.exe profiles artifacts/generic_profile_classes.cmt | sed -E 's/[0-9a-f]{32}/<digest>/g'
  rank-profiles=4
  profile id=rank-profile/v1/prohibited/Generic_profile_classes.0/<digest> digest=<digest> constructor=prohibited#0 path=prohibited uid=Generic_profile_classes.0 independent=false
    parameter 0 identity=parameter:a#0 class=prohibited-negative variance=may-positive=false,may-negative=true,positive=false,negative=true,injective=true
      occurrence type:prohibited[path=prohibited,uid=Generic_profile_classes.0] -> constructor:Prohibited[uid=Generic_profile_classes.1]>field:$0[uid=tuple:Generic_profile_classes.1:0] -> arrow-domain
  profile id=rank-profile/v1/dependent/Generic_profile_classes.2/<digest> digest=<digest> constructor=dependent#1 path=dependent uid=Generic_profile_classes.2 independent=false
    parameter 0 identity=parameter:a#0 class=recursive-dependent variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
      occurrence type:dependent[path=dependent,uid=Generic_profile_classes.2] -> constructor:Dependent[uid=Generic_profile_classes.3]>field:$0[uid=tuple:Generic_profile_classes.3:0]
  profile id=rank-profile/v1/delayed/Generic_profile_classes.4/<digest> digest=<digest> constructor=delayed#2 path=delayed uid=Generic_profile_classes.4 independent=false
    parameter 0 identity=parameter:a#0 class=recursive-dependent variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
      occurrence type:delayed[path=delayed,uid=Generic_profile_classes.4] -> constructor:Delayed[uid=Generic_profile_classes.5]>field:$0[uid=tuple:Generic_profile_classes.5:0] -> arrow-codomain
  profile id=rank-profile/v1/independent/Generic_profile_classes.6/<digest> digest=<digest> constructor=independent#3 path=independent uid=Generic_profile_classes.6 independent=true
    parameter 0 identity=parameter:a#0 class=independently-grounded variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
      occurrence type:independent[path=independent,uid=Generic_profile_classes.6] -> constructor:Independent_value[uid=Generic_profile_classes.8]>field:$0[uid=tuple:Generic_profile_classes.8:0]
    grounding type:independent[path=independent,uid=Generic_profile_classes.6] -> constructor:Independent_ground[uid=Generic_profile_classes.7]
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

A recursively dependent profile admits an actual closed scalar substitution
while keeping the open generic template itself non-independent.

  $ ./recursive_rank_tool.exe profiles artifacts/generic_closed_dependent.cmt | sed -E 's/[0-9a-f]{32}/<digest>/g' | grep -E '^(rank-profiles=|profile id=|  parameter |  application target=dependent_container|    substitution|  grounding|solver-counters)'
  rank-profiles=2
  profile id=rank-profile/v1/dependent_container/Generic_closed_dependent.0/<digest> digest=<digest> constructor=dependent_container#0 path=dependent_container uid=Generic_closed_dependent.0 independent=false
    parameter 0 identity=parameter:a#0 class=recursive-dependent variance=may-positive=true,may-negative=false,positive=true,negative=false,injective=true
  profile id=rank-profile/v1/closed_dependent/Generic_closed_dependent.2/<digest> digest=<digest> constructor=closed_dependent#1 path=closed_dependent uid=Generic_closed_dependent.2 independent=true
    grounding type:closed_dependent[path=closed_dependent,uid=Generic_closed_dependent.2] -> constructor:Closed_base[uid=Generic_closed_dependent.3] -> apply:type:dependent_container[path=dependent_container,uid=Generic_closed_dependent.0] -> substitute:parameter#0=external:int<> -> expand:type:dependent_container[path=dependent_container,uid=Generic_closed_dependent.0] -> constructor:Only[uid=Generic_closed_dependent.1] -> scalar:int
    application target=dependent_container#0 path=dependent_container uid=Generic_closed_dependent.0 profile=rank-profile/v1/dependent_container/Generic_closed_dependent.0/<digest>
      substitution parameter#0=external:int<>
    application target=dependent_container#0 path=dependent_container uid=Generic_closed_dependent.0 profile=rank-profile/v1/dependent_container/Generic_closed_dependent.0/<digest>
      substitution parameter#0=external:int<>
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

Negative laundering pins the complete arrow, tuple, alias, resolved
application, actual-substitution, path, and UID chain. Open/all-recursive,
external, abstract, open, mutable, GADT, higher-kinded, and mutually recursive
generic dependencies fail before VIR or solver creation.

  $ for fixture in generic_negative_launder generic_dependent_open generic_arrow_groundless generic_external generic_abstract generic_open generic_mutable generic_gadt generic_higher_kinded generic_mutual; do OCAML_COLOR=never ./recursive_rank_tool.exe profile-classify "artifacts/$fixture.cmt"; done
  VERO_INVALID_RECURSIVE_RANK: generic rank application uses a prohibited negative parameter in type:t[path=t,uid=Generic_negative_launder.3]; expansion trace: type:t[path=t,uid=Generic_negative_launder.3] -> constructor:Bad[uid=Generic_negative_launder.5]>field:$0[uid=tuple:Generic_negative_launder.5:0] -> apply:type:sink_alias[path=sink_alias,uid=Generic_negative_launder.2] -> substitute:parameter#0=type:t[path=t,uid=Generic_negative_launder.3]<> -> expand:type:sink_alias[path=sink_alias,uid=Generic_negative_launder.2] -> manifest -> apply:type:sink[path=sink,uid=Generic_negative_launder.0] -> substitute:parameter#0=type:t[path=t,uid=Generic_negative_launder.3]<> -> expand:type:sink[path=sink,uid=Generic_negative_launder.0] -> constructor:Sink[uid=Generic_negative_launder.1]>field:$0[uid=tuple:Generic_negative_launder.1:0] -> arrow-domain -> tuple:_ -> apply:type:t[path=t,uid=Generic_negative_launder.3] @ generic_negative_launder.ml:3:0-3:37
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: generic rank grounding fails for open parameters or all-recursive construction paths: type:nonempty[path=nonempty,uid=Generic_dependent_open.0] @ generic_dependent_open.ml:1:0-3:28
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: generic rank grounding fails for open parameters or all-recursive construction paths: type:delayed_recursive[path=delayed_recursive,uid=Generic_arrow_groundless.0] @ generic_arrow_groundless.ml:1:0-2:43
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects unknown or external dependency option; expansion trace: type:external_wrapper[path=external_wrapper,uid=Generic_external.0] -> constructor:Wrapped[uid=Generic_external.2]>field:$0[uid=tuple:Generic_external.2:0] -> external-application:option @ generic_external.ml:1:0-3:24
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects abstract dependencies without a certificate: type:hidden[path=hidden,uid=Generic_abstract.0] @ generic_abstract.ml:1:0-1:14
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects open datatype dependencies: type:extensible[path=extensible,uid=Generic_open.0] @ generic_open.ml:1:0-1:23
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects mutable dependencies: type:mutable_wrapper[path=mutable_wrapper,uid=Generic_mutable.0] @ generic_mutable.ml:1:0-3:34
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects GADT dependencies: type:witness[path=witness,uid=Generic_gadt.0] @ generic_gadt.ml:1:0-3:55
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects higher-kinded or locally polymorphic dependency; expansion trace: type:higher[path=higher,uid=Generic_higher_kinded.0] -> constructor:Higher[uid=Generic_higher_kinded.2]>field:apply[uid=Generic_higher_kinded.1] @ generic_higher_kinded.ml:1:0-2:38
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
  VERO_INVALID_RECURSIVE_RANK: rank profile rejects mutually recursive generic dependencies: type:left[path=left,uid=Generic_mutual.0], type:right[path=right,uid=Generic_mutual.1] @ generic_mutual.ml:1:0-3:25
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

Process-private profile and application authority rejects copied snapshots,
same-name cross-unit replay, forged substitutions/profiles/dependencies, and
mismatched profile instances.

Opaque implementation datatypes cannot mint a profile merely because their
hidden representation is available in the same CMT.

  $ ./recursive_rank_tool.exe profile-classify artifacts/generic_hidden_opaque.cmt
  accepted rank-profiles=0
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

  $ ./recursive_rank_tool.exe profile-attacks artifacts/profile_same_name_a.cmt artifacts/profile_same_name_b.cmt
  same-snapshot authenticated=true
  same-name-cross-unit authenticated=false
  copied-snapshot authenticated=false issued=0
  sealed-application authenticated=true
  forged-substitution authenticated=false
  mismatched-instantiation authenticated=false
  forged-profile authenticated=false
  forged-dependency authenticated=false
  mismatched-profile authenticated=false
  sealed-grounding authenticated=true
  forged-grounding authenticated=false
  in-place-snapshot authenticated=false issued=0
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0

Opaque domain values reject cross-domain VIR and Logic IR projections. A
structurally identical raw-SST copy, mutable recursive child, ownership strings,
or public names cannot recover the process-local issuance binding.

  $ ./recursive_rank_tool.exe attacks artifacts/positive.cmt artifacts/mutable_child.cmt | sed -E 's/[0-9a-f]{32}/<digest>/g'
  same-domain projection (rank[rank-domain/v1/<digest>] first$0)
  cross-domain rejected: rank projection domain rank-domain/v1/<digest> does not contain aggregate tree#1
  raw-sst-copy rank-domains=0
  ownership-altered-raw-sst rank-domains=0
  opacity-forgery rejected: program: invalid semantic SST: incomplete abstract evidence at positive.ml:1:0-3:26
  forged-field rejected: program: invalid semantic SST: constructor field owner must match enclosing variant constructor at positive.ml:3:12-3:15
  mutable-recursive-child rank-domains=0
  logic cross-domain rejected: rank projection for domain "first" member "member" expected First but received Second
  solver-counters smtml=0 z3-contexts=0 z3-solvers=0
