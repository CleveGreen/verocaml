let ( let* ) result continuation =
  match result with
  | Ok value -> continuation value
  | Error _ as error -> error

let member_name aggregate =
  Printf.sprintf "type_%d" aggregate.Vir.aggregate_type_index

let find_sort sorts aggregate =
  match List.assoc_opt aggregate.Vir.aggregate_type_index sorts with
  | Some sort -> Ok sort
  | None ->
      Error
        Logic_ir.
          {
            span = Diagnostic.file_span "<recursive-rank>";
            message =
              Printf.sprintf
                "rank fact references aggregate %s#%d outside its domain"
                aggregate.aggregate_type_name aggregate.aggregate_type_index;
          }

let constructor_key (constructor : Sst.constructor_id) =
  (constructor.constructor_type.type_index, constructor.constructor_index)

let query domain =
  let span = Diagnostic.file_span "<recursive-rank>" in
  let builder = Logic_ir.create () in
  let digest = Vir.rank_domain_digest domain in
  let digest_prefix =
    String.sub digest 0 (min 8 (String.length digest))
  in
  let rec declare_sorts declared = function
    | [] -> Ok (List.rev declared)
    | aggregate :: rest ->
        let* sort =
          Logic_ir.declare_sort builder
            ~name:
              (Printf.sprintf "RankType_%d_%s"
                 aggregate.Vir.aggregate_type_index digest_prefix)
            ~span
        in
        declare_sorts
          ((aggregate.aggregate_type_index, sort) :: declared)
          rest
  in
  let* sorts =
    declare_sorts [] (Vir.rank_domain_component domain)
  in
  let members =
    Vir.rank_domain_component domain
    |> List.map (fun aggregate ->
           ( member_name aggregate,
             Option.get
               (List.assoc_opt aggregate.aggregate_type_index sorts) ))
  in
  let* rank_domain =
    Logic_ir.declare_rank_domain builder
      ~domain_id:(Vir.rank_domain_id domain) ~members ~span
  in
  let constructors =
    Vir.rank_domain_facts domain
    |> List.filter_map (function
         | Vir.Ground_rank_base { constructor; _ }
         | Constructor_rank_nonnegative { constructor }
         | Positive_child_rank_smaller { constructor; _ } ->
             Some constructor)
    |> List.sort_uniq (fun left right ->
           compare (constructor_key left) (constructor_key right))
  in
  let rec declare_predicates declared = function
    | [] -> Ok (List.rev declared)
    | constructor :: rest ->
        let owner =
          {
            Vir.aggregate_type_index =
              constructor.Sst.constructor_type.type_index;
            aggregate_type_name = constructor.constructor_type.type_name;
      aggregate_type_arguments = [];
          }
        in
        let* owner_sort = find_sort sorts owner in
        let* predicate =
          Logic_ir.declare_function builder
            ~name:
              (Printf.sprintf "rank_ctor_t%d_c%d"
                 constructor.constructor_type.type_index
                 constructor.constructor_index)
            ~domain:[ owner_sort ] ~range:Logic_ir.Bool ~span
        in
        declare_predicates
          ((constructor_key constructor, predicate) :: declared)
          rest
  in
  let* predicates = declare_predicates [] constructors in
  let predicate constructor =
    Option.get (List.assoc_opt (constructor_key constructor) predicates)
  in
  let project aggregate value =
    Logic_ir.rank_project ~span rank_domain
      ~member:(member_name aggregate) value
  in
  let owner_type constructor =
    Vir.rank_domain_component domain
    |> List.find_opt (fun aggregate ->
           aggregate.Vir.aggregate_type_index
           = constructor.Sst.constructor_type.type_index)
    |> Option.value
         ~default:
           {
             Vir.aggregate_type_index =
               constructor.Sst.constructor_type.type_index;
             aggregate_type_name = constructor.constructor_type.type_name;
             aggregate_type_arguments = [];
           }
  in
  let rec encode axioms assertions child_index = function
    | [] ->
        Logic_ir.query builder ~axioms:(List.rev axioms)
          ~assertions:(List.rev assertions)
          ~requires:[] ~span
    | fact :: rest -> (
        match fact with
        | Vir.Ground_rank_base { constructor; rank } ->
            let owner = owner_type constructor in
            let* owner_sort = find_sort sorts owner in
            let* witness =
              Logic_ir.declare_function builder
                ~name:
                  (Printf.sprintf "rank_ground_t%d_c%d"
                     constructor.constructor_type.type_index
                     constructor.constructor_index)
                ~domain:[] ~range:owner_sort ~span
            in
            let* witness = Logic_ir.apply ~span witness [] in
            let* projected = project owner witness in
            let* base =
              Logic_ir.equal ~span projected (Logic_ir.int ~span rank)
            in
            encode axioms (base :: assertions) child_index rest
        | Vir.Constructor_rank_nonnegative { constructor } ->
            let owner = owner_type constructor in
            let* owner_sort = find_sort sorts owner in
            let* value =
              Logic_ir.bind builder
                ~name:
                  (Printf.sprintf "parent_t%d_c%d"
                     constructor.constructor_type.type_index
                     constructor.constructor_index)
                ~sort:owner_sort ~span
            in
            let value_term = Logic_ir.bound value in
            let* is_constructor =
              Logic_ir.apply ~span (predicate constructor) [ value_term ]
            in
            let* projected = project owner value_term in
            let* nonnegative =
              Logic_ir.greater_or_equal ~span projected
                (Logic_ir.int ~span Z.zero)
            in
            let* body =
              Logic_ir.implies ~span is_constructor nonnegative
            in
            let qid =
              Printf.sprintf "verocaml.rank.%s.t%d.c%d.nonnegative"
                digest_prefix constructor.constructor_type.type_index
                constructor.constructor_index
            in
            let* axiom =
              Logic_ir.forall builder ~binders:[ value ] ~body
                ~patterns:[ [ is_constructor; projected ] ] ~qid
                ~skid:(qid ^ ".skolem") ~span
            in
            encode (axiom :: axioms) assertions child_index rest
        | Vir.Positive_child_rank_smaller
            { constructor; field; child_path; child_type } ->
            let owner = owner_type constructor in
            let* owner_sort = find_sort sorts owner in
            let* child_sort = find_sort sorts child_type in
            let* selector =
              Logic_ir.declare_function builder
                ~name:
                  (Printf.sprintf "rank_child_t%d_c%d_f%d_p%s_%d"
                     constructor.constructor_type.type_index
                     constructor.constructor_index field.field_index
                     (if child_path = [] then "root"
                      else
                        String.concat "_"
                          (List.map string_of_int child_path))
                     child_index)
                ~domain:[ owner_sort ] ~range:child_sort ~span
            in
            let* value =
              Logic_ir.bind builder
                ~name:
                  (Printf.sprintf "parent_t%d_c%d_f%d_%d"
                     constructor.constructor_type.type_index
                     constructor.constructor_index field.field_index
                     child_index)
                ~sort:owner_sort ~span
            in
            let value_term = Logic_ir.bound value in
            let* is_constructor =
              Logic_ir.apply ~span (predicate constructor) [ value_term ]
            in
            let* child = Logic_ir.apply ~span selector [ value_term ] in
            let* parent_rank = project owner value_term in
            let* child_rank = project child_type child in
            let* smaller =
              Logic_ir.less_than ~span child_rank parent_rank
            in
            let* body =
              Logic_ir.implies ~span is_constructor smaller
            in
            let qid =
              Printf.sprintf "verocaml.rank.%s.t%d.c%d.f%d.%d.child"
                digest_prefix constructor.constructor_type.type_index
                constructor.constructor_index field.field_index child_index
            in
            let* axiom =
              Logic_ir.forall builder ~binders:[ value ] ~body
                ~patterns:[ [ is_constructor; child ] ] ~qid
                ~skid:(qid ^ ".skolem") ~span
            in
            encode (axiom :: axioms) assertions (child_index + 1) rest)
  in
  encode [] [] 0 (Vir.rank_domain_facts domain)
