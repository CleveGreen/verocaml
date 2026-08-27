import hashlib
import pathlib
import re
import sys

root = pathlib.Path(sys.argv[1]).resolve()
src = root / "src"
manifests = root / "test" / "rank_backed_specifications" / "manifests"

baseline = {
    "symbolic_executor_private.ml": 16651,
    "verification_session.ml": 11133,
    "finite_value_registry.ml": 4541,
}
owners = {
    "finite-domain": ("Finite_domain", "finite_value_registry.ml"),
    "direct-candidate": ("Direct_candidate", "finite_value_registry.ml"),
    "finite-expression": ("Finite_expression", "recursive_spec_preservation.ml"),
    "finite-induction": ("Finite_induction", "finite_induction_private.ml"),
    "direct-recursion-induction": ("Direct_recursion_induction", "symbolic_executor_private.ml"),
    "finite-result-integration": ("Finite_result_integration", "symbolic_executor_private.ml"),
    "immutable-fact-integration": ("Immutable_fact_integration", "symbolic_executor_private.ml"),
}
standalone_units = [
    "finite_domain_private", "finite_expression_private",
    "finite_summary_private", "direct_candidate_private",
]


def lines(filename):
    return (src / filename).read_text().splitlines()


def marked_range(label, module_name, filename):
    source = lines(filename)
    begin = [i + 1 for i, line in enumerate(source)
             if f"VERO-065 {label} module: begin" in line]
    end = [i + 1 for i, line in enumerate(source)
           if f"VERO-065 {label} module: end" in line]
    assert len(begin) == len(end) == 1 and begin[0] < end[0], (label, begin, end)
    body = "\n".join(source[begin[0] - 1:end[0]])
    assert re.search(rf"^module {module_name} = struct$", body, re.M), label
    assert end[0] - begin[0] + 1 < 800, (label, begin[0], end[0])
    return filename, begin[0], end[0]


def declaration_ranges(filename, indent=""):
    source = lines(filename)
    let_re = re.compile(rf"^{re.escape(indent)}let (?:rec )?([A-Za-z0-9_]+)\b")
    decl_re = re.compile(rf"^{re.escape(indent)}(?:let|type|module|exception)\b")
    starts = [(i + 1, match.group(1)) for i, line in enumerate(source)
              if (match := let_re.match(line))]
    declarations = [i + 1 for i, line in enumerate(source) if decl_re.match(line)]
    result = {}
    for begin, name in starts:
        end = next((line - 1 for line in declarations if line > begin), len(source))
        result.setdefault(name, []).append((begin, end))
    return result


def top_level_function_ranges(filename):
    source = lines(filename)
    starts = [
        (line_number, match.group(1))
        for line_number, line in enumerate(source, 1)
        if (match := re.match(
            r"^(?:let(?: rec)?|and) ([A-Za-z0-9_]+)\b", line))
    ]
    result = []
    for index, (begin, name) in enumerate(starts):
        end = starts[index + 1][0] - 1 if index + 1 < len(starts) else len(source)
        result.append((name, begin, end))
    return result


def module_functions(label, filename, begin, end):
    source = lines(filename)
    candidates = []
    for line_number in range(begin + 1, end):
        match = re.match(r"^( *)let (?:rec )?([A-Za-z0-9_]+)\b", source[line_number - 1])
        if match:
            candidates.append((len(match.group(1)), line_number, match.group(2)))
    assert candidates, label
    owner_indent = min(indent for indent, _, _ in candidates)
    starts = [(line_number, name) for indent, line_number, name in candidates
              if indent == owner_indent]
    ranges = []
    for index, (first, name) in enumerate(starts):
        last = starts[index + 1][0] - 1 if index + 1 < len(starts) else end - 1
        assert last - first + 1 < 200, (label, name, first, last)
        ranges.append((f"{label}.{name}", filename, first, last))
    return ranges


module_ranges = {}
measured = []
for label, (module_name, filename) in owners.items():
    if filename == "finite_induction_private.ml":
        source = lines(filename)
        assert 0 < len(source) < 800
        module_ranges[label] = (filename, 1, len(source))
        for name, begin, end in top_level_function_ranges(filename):
            assert end - begin + 1 < 200, (label, name, begin, end)
            measured.append((f"{label}.{name}", filename, begin, end))
    else:
        module_ranges[label] = marked_range(label, module_name, filename)
        measured += module_functions(label, *module_ranges[label])

# The remaining substantially rewritten finite functions are named owners. The
# large evaluator is deliberately excluded: it is checked below as thin dispatch.
finite_functions = {
    "recursive_spec_preservation.ml": ["analyze"],
    "finite_value_registry.ml": [
        "valid_receipt", "authenticate", "issue_parent", "derive_child", "consume",
        "issue_result_receipt", "issue_induction_result", "issue_published_result",
        "promote_result", "issue_recursive_spec_result",
    ],
    "verification_session.ml": [
        "finite_result_snapshot_fingerprint", "finite_result_snapshot",
        "record_finite_result_exit", "promote_finite_result",
        "authorize_finite_result_obligations", "complete_finite_result",
        "issue_finite_result", "consume_finite_result",
    ],
    "symbolic_executor_private.ml": [
        "same_direct_recursion", "authorize_finite_formal_call",
        "authorize_frozen_formal_call", "finite_result_demand_dependencies",
        "finite_demand_descriptor", "finite_demand_eligible", "add_finite_sites",
        "intersect_finite_sites", "bind_finite_origin_pattern",
        "bind_finite_authority_pattern", "combine_finite_authority_uses",
        "finite_authority_call", "finite_authority_use",
        "finite_ensures_require_result", "combine_finite_origin_analyses",
        "finite_demand_call", "finite_demand_analyze", "finite_demand_rows",
        "close_finite_demands", "finite_result_snapshot_for_scheduled",
        "authorize_receipt_obligations", "issue_receipt",
    ],
}
for filename, names in finite_functions.items():
    available = declaration_ranges(filename)
    for name in names:
        matches = available.get(name, [])
        assert len(matches) == 1, (filename, name, matches)
        begin, end = matches[0]
        assert end - begin + 1 < 200, (filename, name, begin, end)
        measured.append((f"integration.{name}", filename, begin, end))

# No finite build-surface workaround. VERO-070 through VERO-077 add six exact
# private logical units without adding a public module.
for unit in standalone_units:
    for suffix in (".ml", ".mli"):
        assert not (src / (unit + suffix)).exists(), unit + suffix
logical_unit = "logical_spec_evaluation_private"
logical_filename = logical_unit + ".ml"
logical_source = lines(logical_filename)
assert (src / (logical_unit + ".mli")).exists()
assert len(logical_source) < 1200
logical_authentication_unit = "logical_spec_authentication_private"
logical_authentication_source = lines(logical_authentication_unit + ".ml")
assert (src / (logical_authentication_unit + ".mli")).exists()
assert len(logical_authentication_source) < 800
logical_adt_unit = "logical_adt_evaluation_private"
logical_adt_source = lines(logical_adt_unit + ".ml")
assert len(logical_adt_source) < 800
for name, begin, end in top_level_function_ranges(logical_adt_unit + ".ml"):
    assert end - begin + 1 < 200, (logical_adt_unit, name, begin, end)
    measured.append((f"logical-adt.{name}", logical_adt_unit + ".ml", begin, end))
assert (
    "let authenticate = Logical_spec_authentication_private.authenticate"
    in "\n".join(logical_source)
)
assert "include Logical_adt_evaluation_private" in "\n".join(logical_source)
logical_starts = [
    (line_number, match.group(1))
    for line_number, line in enumerate(logical_source, 1)
    if (match := re.match(r"^(?:let(?: rec)?|and) ([A-Za-z0-9_]+)\b", line))
]
for index, (begin, name) in enumerate(logical_starts):
    end = (
        logical_starts[index + 1][0] - 1
        if index + 1 < len(logical_starts)
        else len(logical_source)
    )
    assert end - begin + 1 < 200, (logical_filename, name, begin, end)
    measured.append((f"logical.{name}", logical_filename, begin, end))

aggregate_unit = "logical_aggregate_term_normalization_private"
aggregate_filename = aggregate_unit + ".ml"
aggregate_source = lines(aggregate_filename)
aggregate_interface = (src / (aggregate_unit + ".mli")).read_text()
assert (src / (aggregate_unit + ".mli")).exists()
assert len(aggregate_source) < 800
aggregate_starts = [
    (line_number, match.group(1))
    for line_number, line in enumerate(aggregate_source, 1)
    if (match := re.match(r"^(?:let(?: rec)?|and) ([A-Za-z0-9_]+)\b", line))
]
for index, (begin, name) in enumerate(aggregate_starts):
    end = (
        aggregate_starts[index + 1][0] - 1
        if index + 1 < len(aggregate_starts)
        else len(aggregate_source)
    )
    assert end - begin + 1 < 200, (aggregate_filename, name, begin, end)
    measured.append((f"aggregate-normalization.{name}", aggregate_filename, begin, end))

dune_source = (src / "dune").read_text()
assert dune_source.count(f"  {aggregate_unit}\n") == 2
assert dune_source.count(f"  {logical_adt_unit}\n") == 2
normalization = "\n".join(aggregate_source)
for required in (
    "type ('opaque, 'exact) observation", "let rec fold", "let rec tag",
    "let rec positional_selector", "constructor_namespace", "exact_argument",
    "integer_selector", "boolean_selector", "aggregate_selector",
    "let rec argument_equal", "and equal",
):
    assert required in normalization or required in aggregate_interface, required
for forbidden in (
    "Logic_ir", "Z3", "Symbolic_executor", "forall", "declare_function",
    "selector_path @",
):
    assert forbidden not in normalization, forbidden
assert "Aggregate_record _" in normalization
assert "source.aggregate_type.aggregate_type_arguments <> []" in normalization

policy_unit = "solver_policy_private"
retained_unit = "retained_model_application_private"
translation_unit = "vir_logic_ir_translation_private"
for label, unit, limit in (
    ("solver-policy", policy_unit, 300),
    ("retained-model", retained_unit, 120),
    ("vir-logic-ir", translation_unit, 1400),
):
    implementation = src / (unit + ".ml")
    interface = src / (unit + ".mli")
    assert implementation.exists() and interface.exists(), unit
    source = implementation.read_text().splitlines()
    assert len(source) < limit, (unit, len(source), limit)
    for name, begin, end in top_level_function_ranges(implementation.name):
        assert end - begin + 1 < 200, (unit, name, begin, end)
        measured.append((f"{label}.{name}", implementation.name, begin, end))
    assert dune_source.count(f"  {unit}\n") == 2, unit

retained_source = (src / (retained_unit + ".ml")).read_text()
retained_interface = (src / (retained_unit + ".mli")).read_text()
assert retained_interface == (
    "val resolve :\n"
    "  Vir.aggregate_term ->\n"
    "  ((string * Vir.recursive_spec_argument list), string) result\n"
)
assert retained_source.count("verocaml_imported_model_") == 1
assert "aggregate_application_identity_matches" in retained_source
assert "cannot be nested as imported model actuals" in retained_source
assert sum(
    (src / filename).read_text().count("verocaml_imported_model_")
    for filename in ("retained_model_application_private.ml", "solver_backend.ml",
                     "vir_logic_ir_translation_private.ml", "z3_bridge.ml")
) == 1

translation_source = (src / (translation_unit + ".ml")).read_text()
translation_interface = (src / (translation_unit + ".mli")).read_text()
for required in (
    "type translation", "val translate :", "requires:Logic_ir.feature list",
    "val query : translation -> Logic_ir.query",
    "val projected :",
):
    assert required in translation_interface, required
for forbidden in ("Z3", "Solver_backend", "Solver_policy", "Smtml"):
    assert forbidden not in translation_source, forbidden
assert "Retained_model_application_private.resolve" in translation_source
assert "Vir.obligation_aggregate_types obligation" in translation_source
assert "Aggregate_logic_symbol_private.named_sort" in translation_source
assert "aggregate type index %d has conflicting names %s and %s" not in translation_source

direct_bridge = (src / "z3_bridge.ml").read_text()
recursive_bridge = (src / "recursive_spec_encoding.ml").read_text()
for backend in (translation_source, recursive_bridge):
    assert backend.count("Logical_aggregate_term_normalization_private.") >= 3
    assert "translate_normalized_observation" not in backend
    for semantic_owner in ("constructor_namespace", "exact_argument",
                           "positional_selector", "argument_equal"):
        assert semantic_owner not in backend, semantic_owner
assert "Logical_aggregate_term_normalization_private.fold" in translation_source
assert len(direct_bridge.splitlines()) < 1700
assert "Retained_model_application_private" not in direct_bridge
assert "Logical_aggregate_term_normalization_private" not in direct_bridge
assert "Vir_logic_ir_translation_private.translate" in direct_bridge
for removed_owner in (
    "translate_aggregate", "translate_integer", "translate_boolean",
    "translate_vir_obligation", "verocaml_imported_model_",
):
    assert removed_owner not in direct_bridge, removed_owner

verification_solver = (src / "verification_solver_private.ml").read_text()
verification_ranges = {
    name: (begin, end)
    for name, begin, end in top_level_function_ranges(
        "verification_solver_private.ml")
}
for helper in (
    "route_obligation", "activations_for_obligation",
    "prepare_recursive_job", "prepare_job", "commit_job", "solve_execution",
):
    begin, end = verification_ranges[helper]
    assert end - begin + 1 < 200, (helper, begin, end)
    measured.append((
        f"verification-solver.{helper}",
        "verification_solver_private.ml", begin, end))
assert "execution.Vir.obligations" in verification_solver[
    verification_solver.index("let solve_execution"):]

parallel_units = (
    "function_frontier_private",
    "function_vc_worker_private",
    "physical_core_count_private",
)
for unit in parallel_units:
    implementation = src / (unit + ".ml")
    interface = src / (unit + ".mli")
    assert dune_source.count(f"  {unit}\n") == 2, unit
    if not implementation.exists() or not interface.exists():
        continue
    source = implementation.read_text().splitlines()
    assert len(source) < 800, (unit, len(source))
    for name, begin, end in top_level_function_ranges(implementation.name):
        assert end - begin + 1 < 200, (unit, name, begin, end)
        measured.append((f"function-frontier.{unit}.{name}",
                         implementation.name, begin, end))
for filename in (
    "z3_bridge.ml",
    "verification_solver_private.ml",
):
    if not (src / filename).exists():
        continue
    for name, begin, end in top_level_function_ranges(filename):
        assert end - begin + 1 < 200, (filename, name, begin, end)
for forbidden in ("Obj.magic", "Domain.spawn", "Unix.fork"):
    assert forbidden not in "\n".join(
        (src / (unit + ".ml")).read_text()
        for unit in parallel_units
        if (src / (unit + ".ml")).exists()
    )
assert dune_source.count("parallel.kernel") == 1
assert dune_source.count("parallel.scheduler") == 1

job_unit = "vc_solver_job_private"
counter_unit = "solver_backend_counter_private"
for unit, limit in ((job_unit, 800), (counter_unit, 200)):
    implementation = src / (unit + ".ml")
    interface = src / (unit + ".mli")
    assert implementation.exists() and interface.exists(), unit
    source = implementation.read_text().splitlines()
    assert len(source) < limit, (unit, len(source), limit)
    for name, begin, end in top_level_function_ranges(implementation.name):
        assert end - begin + 1 < 200, (unit, name, begin, end)
        measured.append((f"prepared-vc.{unit}.{name}",
                         implementation.name, begin, end))
    assert dune_source.count(f"  {unit}\n") == 2, unit
job_source = (src / (job_unit + ".ml")).read_text()
job_interface = (src / (job_unit + ".mli")).read_text()
counter_source = (src / (counter_unit + ".ml")).read_text()
for forbidden in (
    "Verification_session", "proof_activation", "callback", "Z3.context",
    "Z3.Solver", "Smtml", "Domain", "Parallel", "Thread", "scheduler",
):
    assert forbidden not in job_interface, forbidden
assert "Logic_ir.query option" not in job_interface
assert "type contribution" in (src / (counter_unit + ".mli")).read_text()
assert counter_source.count("ref 0") == 2
assert "registry" not in job_source.lower()
assert "registry" not in counter_source.lower()

tuple_unit = "recursive_spec_tuple_match_private"
body_unit = "recursive_spec_body_validation_private"
for unit in (tuple_unit, body_unit):
    implementation = src / (unit + ".ml")
    interface = src / (unit + ".mli")
    assert implementation.exists() and interface.exists(), unit
    source = implementation.read_text().splitlines()
    assert len(source) < 800, (unit, len(source))
    ranges = declaration_ranges(implementation.name)
    for name, occurrences in ranges.items():
        for begin, end in occurrences:
            assert end - begin + 1 < 240, (unit, name, begin, end)
    assert dune_source.count(f"  {unit}\n") == 2, unit

reconstruction_unit = "immutable_aggregate_reconstruction_private"
fact_relevance_unit = "immutable_aggregate_fact_relevance_private"
retry_demand_unit = "recursive_spec_retry_demand_private"
for label, unit in (
    ("immutable-reconstruction", reconstruction_unit),
    ("retry-demand", retry_demand_unit),
):
    implementation = src / (unit + ".ml")
    interface = src / (unit + ".mli")
    assert implementation.exists() and interface.exists(), unit
    source = implementation.read_text().splitlines()
    limit = 650 if unit == reconstruction_unit else 500
    assert len(source) < limit, (unit, len(source), limit)
    starts = [
        (line_number, match.group(1))
        for line_number, line in enumerate(source, 1)
        if (match := re.match(r"^(?:let(?: rec)?|and) ([A-Za-z0-9_]+)\b", line))
    ]
    for index, (begin, name) in enumerate(starts):
        end = starts[index + 1][0] - 1 if index + 1 < len(starts) else len(source)
        assert end - begin + 1 <= 180, (unit, name, begin, end)
        measured.append((f"{label}.{name}", implementation.name, begin, end))
    assert dune_source.count(f"  {unit}\n") == 2, unit

fact_relevance_implementation = src / (fact_relevance_unit + ".ml")
fact_relevance_interface = src / (fact_relevance_unit + ".mli")
assert fact_relevance_implementation.exists()
assert fact_relevance_interface.exists()
fact_relevance_lines = fact_relevance_implementation.read_text().splitlines()
assert len(fact_relevance_lines) < 200, len(fact_relevance_lines)
assert len(fact_relevance_interface.read_text().splitlines()) < 40
fact_relevance_starts = [
    (line_number, match.group(1))
    for line_number, line in enumerate(fact_relevance_lines, 1)
    if (match := re.match(r"^(?:let(?: rec)?|and) ([A-Za-z0-9_]+)\b", line))
]
for index, (begin, name) in enumerate(fact_relevance_starts):
    end = (
        fact_relevance_starts[index + 1][0] - 1
        if index + 1 < len(fact_relevance_starts)
        else len(fact_relevance_lines)
    )
    assert end - begin + 1 < 140, (fact_relevance_unit, name, begin, end)
    measured.append((f"immutable-fact-relevance.{name}",
                     fact_relevance_implementation.name, begin, end))
assert dune_source.count(f"  {fact_relevance_unit}\n") == 2

tuple_planner = (src / (tuple_unit + ".ml")).read_text()
body_validation = (src / (body_unit + ".ml")).read_text()
sst_validation = (src / "sst_validation_private.ml").read_text()
spec_unfolding = (src / "spec_unfolding_private.ml").read_text()
recursive_encoding = (src / "recursive_spec_encoding.ml").read_text()
adapter = (src / "typedtree_adapter_private.ml").read_text()
executor_source = (src / "symbolic_executor_private.ml").read_text()
reconstruction = (src / (reconstruction_unit + ".ml")).read_text()
reconstruction_interface = (src / (reconstruction_unit + ".mli")).read_text()
fact_relevance = fact_relevance_implementation.read_text()
fact_relevance_api = fact_relevance_interface.read_text()
retry_demand = (src / (retry_demand_unit + ".ml")).read_text()
for required in (
    "Expected_tuple_value", "Expected_tuple_pattern", "Tuple_type_mismatch",
    "Tuple_label_mismatch", "Tuple_arity_mismatch", "Tuple_nesting_mismatch",
    "Unsupported_leaf_type", "exact_tuple_type", "same_labels",
):
    assert required in tuple_planner, required
assert body_validation.count("Recursive_spec_tuple_match_private.plan") == 2
assert spec_unfolding.count("Recursive_spec_tuple_match_private.plan") == 1
assert recursive_encoding.count("Recursive_spec_tuple_match_private.plan") == 2
assert "ground_bind_tuple_pattern" in recursive_encoding
assert "translate_match_pattern" in recursive_encoding
assert "bind_match_pattern parametric_adts translate" in spec_unfolding
assert "validate_recursive_first_order_body" not in sst_validation
assert sst_validation.count("Recursive_spec_body_validation_private.validate") == 3
assert "Sst.Tuple_pattern components" in adapter
assert "Sst.Tuple_value components" in adapter
assert "Recursive_spec_tuple_match_private" not in adapter
assert sum(len(lines(name)) for name in (
    "sst_validation_private.ml", "spec_unfolding_private.ml",
    "recursive_spec_encoding.ml",
)) <= 12800
assert len(lines("symbolic_executor_private.ml")) + len(lines(
    "recursive_spec_encoding.ml"
)) <= 18000
assert "expression_has_aggregate_equality" not in executor_source
assert "exact_aggregate_construction_equality" not in executor_source
assert "direct_goal_aggregate_application" not in recursive_encoding
assert reconstruction.count("Finite_domain.deeply_immutable_type") == 1
for name in (
    "is_exact_aggregate_construction_equality",
    "goal_has_aggregate_equality",
    "facts_relevant_to_terms",
):
    assert re.search(rf"^let(?: rec)? {name}\b", fact_relevance,
                     re.MULTILINE), name
    assert f"val {name}" in fact_relevance_api, name
    assert name not in reconstruction, name
    assert name not in reconstruction_interface, name
assert executor_source.count("Immutable_aggregate_fact_relevance_private.") == 2
assert recursive_encoding.count(
    "Immutable_aggregate_fact_relevance_private") == 1
fact_relevance_modules = set(re.findall(
    r"\b([A-Z][A-Za-z0-9_]*)\.", fact_relevance))
assert fact_relevance_modules <= {
    "List", "Parametric_logic_private", "Symbolic_application_private", "Vir",
}, fact_relevance_modules
for forbidden in (
    "verification_session", "logic_ir", "z3", "receipt", "rank", "lineage",
    "reachability", "ownership",
):
    assert forbidden not in reconstruction.lower(), forbidden
assert "Recursive_spec_retry_demand_private.of_goal" in executor_source
assert "Recursive_spec_retry_demand_private.of_goal" in recursive_encoding
assert retry_demand.count("Aggregate_recursive_spec_application") == 1
for filename, names in {
    "spec_unfolding_private.ml": [
        "bind_match_pattern", "translate_boolean_match", "translate",
    ],
    "recursive_spec_encoding.ml": [
        "translate_match_pattern", "translate_match_body", "translate_body",
        "ground_bind_tuple_pattern", "ground_sst_expression",
    ],
}.items():
    available = declaration_ranges(filename)
    for name in names:
        occurrences = available.get(name, [])
        assert len(occurrences) == 1, (filename, name, occurrences)
        begin, end = occurrences[0]
        assert end - begin + 1 < 350, (filename, name, begin, end)

expected_manifests = {
    "installed-paths.manifest": "4be24f090dc87fa4774121dcbe5bfeee2d2b61722f04bc56187fc3cf11cd0a65",
    "installed-interfaces.manifest": "7e60ba2fd5c04c15848dea3e3f9e5ce0082bc38353d155da8bac5e982e51904f",
    "installed-public-modules.manifest": "c7cd2560be752ccd8dd0f769e8188c153799c982a9000cb6fb37c636de8a11e8",
}
for filename, expected in expected_manifests.items():
    contents = (manifests / filename).read_bytes()
    assert hashlib.sha256(contents).hexdigest() == expected, filename
    inventory = contents.decode().lower()
    assert all(unit not in inventory for unit in standalone_units), filename
    if filename != "installed-public-modules.manifest":
        assert "finite_induction_private" in inventory
    if filename == "installed-public-modules.manifest":
        assert logical_unit not in inventory
        assert logical_authentication_unit not in inventory
        assert logical_adt_unit not in inventory
        assert aggregate_unit not in inventory
        assert tuple_unit not in inventory
        assert body_unit not in inventory
        assert fact_relevance_unit not in inventory
        assert reconstruction_unit not in inventory
        assert retry_demand_unit not in inventory
        assert job_unit not in inventory
        assert counter_unit not in inventory
    else:
        assert logical_unit in inventory
        assert logical_authentication_unit in inventory
        assert logical_adt_unit in inventory
        assert aggregate_unit in inventory
        assert tuple_unit in inventory
        assert body_unit in inventory
        assert fact_relevance_unit in inventory
        assert reconstruction_unit in inventory
        assert retry_demand_unit in inventory
        assert job_unit in inventory
        assert counter_unit in inventory
        assert all(unit in inventory for unit in parallel_units)
assert len((manifests / "installed-paths.manifest").read_text().splitlines()) == 920
assert len((manifests / "installed-interfaces.manifest").read_text().splitlines()) == 222
assert len((manifests / "installed-public-modules.manifest").read_text().splitlines()) == 21

# Preserve the finite registry reduction and the aggregate reduction. Callback
# session dispatch is covered by VERO-103's dedicated concentration aggregate.
current = {name: len(lines(name)) for name in baseline}
assert current["finite_value_registry.ml"] < baseline["finite_value_registry.ml"]
assert sum(current.values()) < sum(baseline.values()), current

production_files = [
    "finite_value_registry.ml", "finite_value_registry.mli",
    "recursive_spec_preservation.ml", "recursive_spec_preservation.mli",
    "symbolic_executor_private.ml", "termination.ml",
    "verification_session.ml", "verification_session.mli", "verification_pipeline.ml",
]
production = "\n".join((src / name).read_text() for name in production_files)
for symbol in (
    "direct_self_finite_composition_summary", "direct_self_finite_transfer_summary",
    "direct_self_observer_permit", "direct_self_composition_step",
    "result_coverage_identity", "result_coverage_key", "finite_manifest_coverage",
    "finite_authority_coverage", "finalize_finite_result", "demand_matrix",
    "force_direct_self_transfer_scheduler_edge_for_testing", "Finite_summary",
    "publish_group", "register_member", "group_members", "finite_summary_group",
    "prepare_mutual",
):
    assert not re.search(rf"\b{re.escape(symbol)}\b", production), symbol
assert not re.search(r"\bdirect_self_(?:finite|transfer|observer|composition)[A-Za-z0-9_]*\b", production)

registry = (src / "finite_value_registry.ml").read_text()
preservation = (src / "recursive_spec_preservation.ml").read_text()
executor = (src / "symbolic_executor_private.ml").read_text()
termination = (src / "termination.ml").read_text()
session = (src / "verification_session.ml").read_text()

# The shared owner authenticates all exact premises and owns the rejection
# decisions. Ordinary construction/result and immutable recursive-Spec routes
# enter the same judgment; frozen-spine does not.
checker_range = module_ranges["finite-expression"]
checker = "\n".join(lines(checker_range[0])[checker_range[1] - 1:checker_range[2]])
for required in (
    "authenticate_fact", "authenticate_node", "authenticate_origin",
    "authenticate_authority", "Missing_fact", "rejected",
    "Immutable_constructor", "Immutable_record", "Immutable_projection",
    "Immutable_pattern", "All_feasible_branches", "Completed_nonrecursive_summary",
    "Strictly_smaller_direct_call", "Immutable_recursive_spec_result",
):
    assert required in checker, required
assert "origin_kind -> 'node -> 'fact" in (
    src / "recursive_spec_preservation.mli"
).read_text()
assert "projection/pattern origin is not the exact finite parent" in checker
assert "let _ =" not in checker
assert registry.count("Recursive_spec_preservation.Finite_expression.derive") >= 4
assert "Finite_value_registry" not in preservation
for source in ("Exact_let", "Exact_alias", "Immutable_constructor", "Immutable_record",
               "Immutable_projection", "Immutable_pattern", "All_feasible_branches",
               "Strictly_smaller_direct_call"):
    assert source in preservation, source
frozen = re.search(r"if is_frozen then(?P<body>.*?)^        else$", preservation, re.M | re.S)
assert frozen and "check_immutable_recursive_spec" not in frozen.group("body")
assert "Frozen_spine_direct_edge _" in executor

# Singular exact direct candidate: completion and publication are separate,
# publication authenticates all recorded exits and obligations, and consumption
# mints a fresh exact caller result fact.
direct = "\n".join(lines(module_ranges["direct-candidate"][0])[
    module_ranges["direct-candidate"][1] - 1:module_ranges["direct-candidate"][2]])
for required in ("body_snapshot", "profile_snapshot", "record_exit", "authorize_obligations",
                 "exit_fingerprint", "exits are already sealed", "all_verified",
                 "publish", "consume", "call_path_digest", "result_snapshot"):
    assert required in direct, required
assert "Direct_candidate.publish" in session
assert "Direct_candidate.consume" in session
assert "issue_published_result" in session

# Mutual recursion is rejected in precheck and cannot prepare finite IH or publish.
assert re.search(r"let precheck.*?reject_mutual", termination, re.S)
assert re.search(r"\| Some Mutual -> assert false", termination)
assert "same_direct_recursion" in executor

# Evaluate contains narrow dispatch only, not finite traversal/lifecycle authority.
evaluate = re.search(r"^let rec evaluate\b(?P<body>.*?)^let lower_summary\b", executor, re.M | re.S)
assert evaluate, "evaluate range"
eval_body = evaluate.group("body")
for dispatch in ("Direct_recursion_induction.prepare", "Direct_recursion_induction.bind_hypothesis",
                 "Direct_recursion_induction.issue_result", "Finite_result_integration.consume_published",
                 "Immutable_fact_integration.issue_finite_construction",
                 "Immutable_fact_integration.derive_finite_pattern"):
    assert dispatch in eval_body, dispatch
assert "Finite_result_integration.record_materialized_exit" in executor
for forbidden in ("exact_finite_actual_receipts", "Verification_session.consume_finite_result",
                  "Verification_session.record_finite_result_exit",
                  "Verification_session.promote_finite_result",
                  "Finite_value_registry.issue_induction_result"):
    assert forbidden not in eval_body, forbidden

summary = ",".join(f"{label}={filename}:{begin}-{end}"
                   for label, (filename, begin, end) in module_ranges.items())
ranges = ",".join(f"{name}={filename}:{begin}-{end}"
                  for name, filename, begin, end in measured)
print("finite-owners=" + summary)
print("legacy-lines=" + ",".join(f"{name}:{baseline[name]}->{current[name]}" for name in baseline)
      + f",aggregate:{sum(baseline.values())}->{sum(current.values())}")
print(f"measured-functions={len(measured)} ranges-sha256={hashlib.sha256(ranges.encode()).hexdigest()}")
print("measured-function-ranges=" + ranges)
print("shared-checker=ordinary+immutable-recursive-spec frozen-spine=separate")
print("candidate-lifecycle=singular/all-exits/all-obligations mutual=unsupported")
print("evaluate=thin-finite-dispatch finite-compiled-units=1 logical-private-units=18 aggregate-normalizer=shared-direct+recursive tuple-match=ephemeral-shared reconstruction=immutable-only fact-relevance=pure-two-consumer retry-demand=shared-exact-view")
print("installed-manifests=" + ",".join(f"{name}:{digest}" for name, digest in expected_manifests.items()))
