type span = Diagnostic.span

type feature =
  | Named_sorts
  | Uninterpreted_functions
  | Linear_integer_arithmetic
  | Quantifiers
  | Explicit_patterns
  | Quantifier_ids
  | Models
  | Nonlinear_integer_arithmetic
  | Algebraic_datatypes
  | Bit_vectors
  | Int_bitvector_conversions

module Feature_order = struct
  type t = feature

  let compare = Stdlib.compare
end

module Feature_set = Set.Make (Feature_order)

type owner = unit ref

type named_sort = {
  named_sort_owner : owner;
  named_sort_index : int;
  named_sort_name : string;
  named_sort_span : span;
}

type sort = Int | Bool | Bv of Bv_width.t | Named of named_sort

type function_symbol = {
  function_owner : owner;
  function_index : int;
  function_name : string;
  function_domain : sort list;
  function_range : sort;
  function_span : span;
}

type datatype_field_sort = Field_sort of sort | Recursive_self

type datatype_field_spec = {
  field_id : string;
  field_name : string;
  field_sort : datatype_field_sort;
}

type datatype_constructor_spec = {
  constructor_id : string;
  constructor_name : string;
  recognizer_id : string;
  fields : datatype_field_spec list;
}

type datatype_field = {
  datatype_field_id : string;
  datatype_field_sort : datatype_field_sort;
  datatype_field_symbol : function_symbol;
}

type datatype_constructor = {
  datatype_constructor_id : string;
  datatype_constructor_symbol : function_symbol;
  datatype_recognizer_symbol : function_symbol;
  datatype_fields : datatype_field list;
}

type datatype = {
  datatype_id : string;
  datatype_scc_id : string;
  datatype_sort : named_sort;
  datatype_constructors : datatype_constructor list;
}

type rank_domain = {
  rank_domain_owner : owner;
  rank_domain_id : string;
  rank_domain_members : (string * sort * function_symbol) list;
  rank_domain_span : span;
}

type binder = {
  binder_owner : owner;
  binder_index : int;
  binder_name : string;
  binder_sort : sort;
  binder_span : span;
}

module Int_set = Set.Make (Int)

type term = {
  term_sort : sort;
  term_span : span;
  term_node : term_node;
  free_binders : Int_set.t;
  used_functions : Int_set.t;
  term_features : Feature_set.t;
  term_owner : owner option;
}

and bv_projection = {
  bv_projection_owner : owner;
  bv_projection_identity : string;
  bv_projection_width : Bv_width.t;
  bv_projection_term : term;
  bv_projection_span : span;
}

and user_quantifier = {
  user_quantifier_binders : binder list;
  user_quantifier_body : term;
  user_quantifier_trigger : term option;
  user_quantifier_qid : string;
  user_quantifier_skid : string;
  user_quantifier_span : span;
}

and term_node =
  | Integer of Z.t
  | Boolean of bool
  | Bound of binder
  | Apply of function_symbol * term list
  | Rank_project of rank_domain * string * function_symbol * term
  | Add of term * term
  | Subtract of term * term
  | Negate of term
  | Multiply of term * term
  | Scale of Z.t * term
  | Less_than of term * term
  | Less_or_equal of term * term
  | Greater_than of term * term
  | Greater_or_equal of term * term
  | Equal of term * term
  | Distinct of term * term
  | Not of term
  | And of term list
  | Or of term list
  | Implies of term * term
  | Forall_term of user_quantifier
  | Exists_term of user_quantifier
  | Ite of term * term * term
  | Bv_literal of Bv_value.t
  | Bv_eq of term * term
  | Bv_distinct of term * term
  | Bv_add_mod of term * term
  | Bv_sub_mod of term * term
  | Bv_not of term
  | Bv_and of term * term
  | Bv_or of term * term
  | Bv_xor of term * term
  | Bv_ult of term * term
  | Bv_ule of term * term
  | Bv_ugt of term * term
  | Bv_uge of term * term
  | Bv_slt of term * term
  | Bv_sle of term * term
  | Bv_sgt of term * term
  | Bv_sge of term * term
  | Bv_to_int_unsigned of term
  | Bv_to_int_signed of term
  | Int_to_bv_mod of Bv_width.t * term

type axiom = {
  axiom_owner : owner;
  axiom_binders : binder list;
  axiom_body : term;
  axiom_patterns : term list list;
  axiom_qid : string;
  axiom_skid : string;
  axiom_span : span;
  axiom_features : Feature_set.t;
}

type declaration =
  | Sort_declaration of named_sort
  | Function_declaration of function_symbol

type query = {
  query_declarations : declaration list;
  query_datatypes : datatype list;
  query_axioms : axiom list;
  query_assertions : term list;
  query_bv_projections : bv_projection list;
  query_requirements : feature list;
  query_span : span;
}

type builder = {
  owner : owner;
  mutable next_declaration : int;
  mutable next_binder : int;
  mutable declarations_reversed : declaration list;
  mutable datatypes_reversed : datatype list;
  mutable sort_names : string list;
  mutable function_names : string list;
  mutable rank_domain_ids : string list;
  mutable datatype_ids : string list;
}

type error = {
  span : span;
  message : string;
}

let create () =
  {
    owner = ref ();
    next_declaration = 0;
    next_binder = 0;
    declarations_reversed = [];
    datatypes_reversed = [];
    sort_names = [];
    function_names = [];
    rank_domain_ids = [];
    datatype_ids = [];
  }

let error span format =
  Printf.ksprintf (fun message -> Error { span; message }) format

let valid_name value =
  String.length value > 0
  && String.for_all
       (fun character ->
         let code = Char.code character in
         code >= 0x20 && code <> 0x7f)
       value

let validate_name ~kind ~span name =
  if valid_name name then Ok ()
  else error span "%s name must be nonempty and contain no control characters" kind

let same_owner left right = left == right

let sort_equal left right =
  match (left, right) with
  | Int, Int | Bool, Bool -> true
  | Bv left, Bv right -> Bv_width.equal left right
  | Named left, Named right ->
      same_owner left.named_sort_owner right.named_sort_owner
      && Int.equal left.named_sort_index right.named_sort_index
  | (Int | Bool | Bv _ | Named _), (Int | Bool | Bv _ | Named _) -> false

let sort_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Bv width -> Printf.sprintf "BV<%s>" (Bv_width.to_string width)
  | Named sort -> sort.named_sort_name

let sort_belongs_to owner = function
  | Int | Bool | Bv _ -> true
  | Named sort -> same_owner owner sort.named_sort_owner

let validate_sort ~span = function
  | Int | Bool | Named _ -> Ok ()
  | Bv width ->
      Bv_width.authenticate_bound
        (Bv_backend_capability_receipt_private.capability ())
        width
      |> Result.map_error (fun message -> { span; message })

let sort_features = function
  | Bv _ -> Feature_set.singleton Bit_vectors
  | Int | Bool | Named _ -> Feature_set.empty

let declare_sort builder ~name ~span =
  match validate_name ~kind:"sort" ~span name with
  | Error _ as error -> error
  | Ok () ->
      if List.mem name builder.sort_names then
        error span "sort %S is already declared" name
      else
        let sort =
          {
            named_sort_owner = builder.owner;
            named_sort_index = builder.next_declaration;
            named_sort_name = name;
            named_sort_span = span;
          }
        in
        builder.next_declaration <- builder.next_declaration + 1;
        builder.declarations_reversed <-
          Sort_declaration sort :: builder.declarations_reversed;
        builder.sort_names <- name :: builder.sort_names;
        Ok (Named sort)

let declare_function builder ~name ~domain ~range ~span =
  match validate_name ~kind:"function" ~span name with
  | Error _ as error -> error
  | Ok () ->
      if List.mem name builder.function_names then
        error span "function %S is already declared" name
      else if
        not
          (List.for_all (sort_belongs_to builder.owner) (range :: domain))
      then
        error span "function %S uses a named sort from another logic signature" name
      else
        match
          List.find_map
            (fun sort ->
              match validate_sort ~span sort with
              | Ok () -> None
              | Error error -> Some error)
            (range :: domain)
        with
        | Some error -> Error error
        | None ->
        let symbol =
          {
            function_owner = builder.owner;
            function_index = builder.next_declaration;
            function_name = name;
            function_domain = domain;
            function_range = range;
            function_span = span;
          }
        in
        builder.next_declaration <- builder.next_declaration + 1;
        builder.declarations_reversed <-
          Function_declaration symbol :: builder.declarations_reversed;
        builder.function_names <- name :: builder.function_names;
        Ok symbol

let unique values =
  List.length values = List.length (List.sort_uniq String.compare values)

let validate_datatype_specs builder ~datatype_id ~sort_name constructors ~span =
  let constructor_names =
    List.concat_map
      (fun constructor ->
        constructor.constructor_name :: constructor.recognizer_id
        :: List.map (fun field -> field.field_name) constructor.fields)
      constructors
  in
  let constructor_ids =
    List.map (fun constructor -> constructor.constructor_id) constructors
  in
  let field_ids =
    List.concat_map
      (fun constructor ->
        List.map (fun field -> field.field_id) constructor.fields)
      constructors
  in
  if constructors = [] then error span "datatype %S has no constructors" datatype_id
  else if
    not
      (List.for_all valid_name
         (datatype_id :: sort_name :: constructor_ids @ field_ids
        @ constructor_names))
  then error span "datatype %S contains an invalid portable identity" datatype_id
  else if List.mem datatype_id builder.datatype_ids then
    error span "datatype %S is already declared" datatype_id
  else if List.mem sort_name builder.sort_names then
    error span "sort %S is already declared" sort_name
  else if not (unique constructor_ids) then
    error span "datatype %S contains duplicate constructor identities" datatype_id
  else if not (unique field_ids) then
    error span "datatype %S contains duplicate field identities" datatype_id
  else if
    (not (unique constructor_names))
    || List.exists (fun name -> List.mem name builder.function_names)
         constructor_names
  then error span "datatype %S contains a duplicate logical symbol" datatype_id
  else
    match
      List.find_opt
        (fun field ->
          match field.field_sort with
          | Recursive_self -> false
          | Field_sort sort ->
              (not (sort_belongs_to builder.owner sort))
              || Result.is_error (validate_sort ~span sort))
        (List.concat_map (fun constructor -> constructor.fields) constructors)
    with
    | Some _ -> error span "datatype %S uses a sort from another signature" datatype_id
    | None -> Ok constructor_names

let declare_datatype builder ~datatype_id ~scc_id ~sort_name ~constructors
    ~span =
  match validate_name ~kind:"datatype SCC" ~span scc_id with
  | Error _ as error -> error
  | Ok () -> (
      match
        validate_datatype_specs builder ~datatype_id ~sort_name constructors
          ~span
      with
      | Error _ as error -> error
      | Ok function_names ->
          let named_sort =
            { named_sort_owner = builder.owner;
              named_sort_index = builder.next_declaration;
              named_sort_name = sort_name;
              named_sort_span = span }
          in
          builder.next_declaration <- builder.next_declaration + 1;
          let datatype_sort = Named named_sort in
          let make_function name domain range =
            let function_ =
              { function_owner = builder.owner;
                function_index = builder.next_declaration;
                function_name = name;
                function_domain = domain;
                function_range = range;
                function_span = span }
            in
            builder.next_declaration <- builder.next_declaration + 1;
            function_
          in
          let declare_constructor spec =
            let field_sorts =
              List.map
                (fun field ->
                  match field.field_sort with
                  | Recursive_self -> datatype_sort
                  | Field_sort sort -> sort)
                spec.fields
            in
            let constructor_symbol =
              make_function spec.constructor_name field_sorts datatype_sort
            and recognizer_symbol =
              make_function spec.recognizer_id [ datatype_sort ] Bool
            in
            let fields =
              List.map2
                (fun field sort ->
                  { datatype_field_id = field.field_id;
                    datatype_field_sort = field.field_sort;
                    datatype_field_symbol =
                      make_function field.field_name [ datatype_sort ] sort })
                spec.fields field_sorts
            in
            { datatype_constructor_id = spec.constructor_id;
              datatype_constructor_symbol = constructor_symbol;
              datatype_recognizer_symbol = recognizer_symbol;
              datatype_fields = fields }
          in
          let datatype =
            { datatype_id;
              datatype_scc_id = scc_id;
              datatype_sort = named_sort;
              datatype_constructors = List.map declare_constructor constructors }
          in
          let function_declarations =
            datatype.datatype_constructors
            |> List.concat_map (fun constructor ->
                   Function_declaration
                     constructor.datatype_constructor_symbol
                   :: Function_declaration
                        constructor.datatype_recognizer_symbol
                   :: List.map
                        (fun field ->
                          Function_declaration
                            field.datatype_field_symbol)
                        constructor.datatype_fields)
          in
          builder.declarations_reversed <-
            List.rev_append
              (Sort_declaration named_sort :: function_declarations)
              builder.declarations_reversed;
          builder.datatypes_reversed <-
            datatype :: builder.datatypes_reversed;
          builder.sort_names <- sort_name :: builder.sort_names;
          builder.function_names <-
            List.rev_append function_names builder.function_names;
          builder.datatype_ids <- datatype_id :: builder.datatype_ids;
          Ok datatype)

let datatype_sort datatype = Named datatype.datatype_sort
let datatype_constructors datatype = datatype.datatype_constructors
let datatype_constructor_symbol constructor =
  constructor.datatype_constructor_symbol
let datatype_recognizer_symbol constructor =
  constructor.datatype_recognizer_symbol
let datatype_fields constructor = constructor.datatype_fields
let datatype_field_symbol field = field.datatype_field_symbol

let logic_identifier value =
  String.map
    (fun character ->
      match character with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> character
      | _ -> '_')
    value

let declare_rank_domain builder ~domain_id ~members ~span =
  let rec unique seen = function
    | [] -> true
    | (name, _) :: rest ->
        not (List.mem name seen) && unique (name :: seen) rest
  in
  if not (valid_name domain_id) then
    error span
      "rank domain id must be nonempty and contain no control characters"
  else if List.mem domain_id builder.rank_domain_ids then
    error span "rank domain %S is already declared" domain_id
  else if members = [] then
    error span "rank domain %S must contain at least one member" domain_id
  else if not (unique [] members) then
    error span "rank domain %S contains duplicate member identities" domain_id
  else if
    List.exists
      (fun (_, sort) ->
        match sort with
        | Named _ -> not (sort_belongs_to builder.owner sort)
        | Int | Bool | Bv _ -> true)
      members
  then
    error span
      "rank domain %S members must be named sorts from this logic signature"
      domain_id
  else
    let rec declare declared = function
      | [] ->
          let domain =
            {
              rank_domain_owner = builder.owner;
              rank_domain_id = domain_id;
              rank_domain_members = List.rev declared;
              rank_domain_span = span;
            }
          in
          builder.rank_domain_ids <- domain_id :: builder.rank_domain_ids;
          Ok domain
      | (member, sort) :: rest ->
          let name =
            Printf.sprintf "rank_d%d_%s" builder.next_declaration
              (logic_identifier member)
          in
          (match
             declare_function builder ~name ~domain:[ sort ] ~range:Int ~span
           with
          | Error _ as error -> error
          | Ok function_ ->
              declare ((member, sort, function_) :: declared) rest)
    in
    declare [] members

let bind builder ~name ~sort ~span =
  match validate_name ~kind:"binder" ~span name with
  | Error _ as error -> error
  | Ok () ->
      if not (sort_belongs_to builder.owner sort) then
        error span "binder %S uses a named sort from another logic signature" name
      else
        match validate_sort ~span sort with
        | Error _ as error -> error
        | Ok () ->
        let binder =
          {
            binder_owner = builder.owner;
            binder_index = builder.next_binder;
            binder_name = name;
            binder_sort = sort;
            binder_span = span;
          }
        in
        builder.next_binder <- builder.next_binder + 1;
        Ok binder

let owner_of_terms terms =
  let rec loop owner = function
    | [] -> Ok owner
    | { term_owner = None; _ } :: rest -> loop owner rest
    | { term_owner = Some candidate; term_span; _ } :: rest -> (
        match owner with
        | None -> loop (Some candidate) rest
        | Some owner when same_owner owner candidate -> loop (Some owner) rest
        | Some _ ->
            error term_span
              "term combines declarations or binders from different logic signatures")
  in
  loop None terms

let merge_sets project terms =
  List.fold_left
    (fun combined term -> Int_set.union combined (project term))
    Int_set.empty terms

let merge_features terms =
  List.fold_left
    (fun combined term -> Feature_set.union combined term.term_features)
    Feature_set.empty terms

let binder_sort_features binders =
  List.fold_left
    (fun features binder ->
      Feature_set.union features (sort_features binder.binder_sort))
    Feature_set.empty binders

let make_term ~sort ~span ~node ~features terms =
  match owner_of_terms terms with
  | Error _ as error -> error
  | Ok owner ->
      Ok
        {
          term_sort = sort;
          term_span = span;
          term_node = node;
          free_binders = merge_sets (fun term -> term.free_binders) terms;
          used_functions = merge_sets (fun term -> term.used_functions) terms;
          term_features =
            Feature_set.union features (merge_features terms);
          term_owner = owner;
        }

let int ~span value =
  {
    term_sort = Int;
    term_span = span;
    term_node = Integer value;
    free_binders = Int_set.empty;
    used_functions = Int_set.empty;
    term_features = Feature_set.empty;
    term_owner = None;
  }

let bool ~span value =
  {
    term_sort = Bool;
    term_span = span;
    term_node = Boolean value;
    free_binders = Int_set.empty;
    used_functions = Int_set.empty;
    term_features = Feature_set.empty;
    term_owner = None;
  }

let bv_literal ~span value =
  let width = value.Bv_value.width in
  match validate_sort ~span (Bv width) with
  | Error _ as error -> error
  | Ok () ->
      Ok
        { term_sort = Bv width;
          term_span = span;
          term_node = Bv_literal value;
          free_binders = Int_set.empty;
          used_functions = Int_set.empty;
          term_features = Feature_set.singleton Bit_vectors;
          term_owner = None }

let bound binder =
  {
    term_sort = binder.binder_sort;
    term_span = binder.binder_span;
    term_node = Bound binder;
    free_binders = Int_set.singleton binder.binder_index;
    used_functions = Int_set.empty;
    term_features =
      (match binder.binder_sort with
      | Named _ -> Feature_set.singleton Named_sorts
      | Bv _ -> Feature_set.singleton Bit_vectors
      | Int | Bool -> Feature_set.empty);
    term_owner = Some binder.binder_owner;
  }

let rec equal_sort_lists left right =
  match (left, right) with
  | [], [] -> true
  | left :: lefts, right :: rights ->
      sort_equal left right && equal_sort_lists lefts rights
  | [], _ :: _ | _ :: _, [] -> false

let apply ~span function_ arguments =
  let actual_sorts = List.map (fun term -> term.term_sort) arguments in
  if not (equal_sort_lists function_.function_domain actual_sorts) then
    error span "function %S expects (%s) but was applied to (%s)"
      function_.function_name
      (String.concat ", " (List.map sort_to_string function_.function_domain))
      (String.concat ", " (List.map sort_to_string actual_sorts))
  else
    match owner_of_terms arguments with
    | Error _ as error -> error
    | Ok (Some owner) when not (same_owner owner function_.function_owner) ->
        error span
          "function %S is applied to a term from another logic signature"
          function_.function_name
    | Ok _ ->
        let sort_features =
          List.fold_left
            (fun features sort ->
              let features = Feature_set.union features (sort_features sort) in
              match sort with
              | Named _ -> Feature_set.add Named_sorts features
              | Int | Bool | Bv _ -> features)
            Feature_set.empty
            (function_.function_range :: function_.function_domain)
        in
        let features =
          Feature_set.add Uninterpreted_functions sort_features
        in
        (match
           make_term ~sort:function_.function_range ~span
             ~node:(Apply (function_, arguments)) ~features arguments
         with
        | Error _ as error -> error
        | Ok term ->
            Ok
              {
                term with
                used_functions =
                  Int_set.add function_.function_index term.used_functions;
                term_owner = Some function_.function_owner;
              })

let rank_project ~span domain ~member value =
  match
    List.find_opt
      (fun (candidate, _, _) -> String.equal candidate member)
      domain.rank_domain_members
  with
  | None ->
      error span "rank domain %S has no member %S" domain.rank_domain_id member
  | Some (_, expected, function_) ->
      if not (same_owner domain.rank_domain_owner function_.function_owner) then
        error span "rank domain %S has foreign projection authority"
          domain.rank_domain_id
      else if not (sort_equal expected value.term_sort) then
        error span
          "rank projection for domain %S member %S expected %s but received %s"
          domain.rank_domain_id member (sort_to_string expected)
          (sort_to_string value.term_sort)
      else
        let features =
          Feature_set.of_list
            [
              Named_sorts;
              Uninterpreted_functions;
              Linear_integer_arithmetic;
            ]
        in
        match
          make_term ~sort:Int ~span
            ~node:(Rank_project (domain, member, function_, value))
            ~features [ value ]
        with
        | Error _ as error -> error
        | Ok term ->
            Ok
              {
                term with
                used_functions =
                  Int_set.add function_.function_index term.used_functions;
                term_owner = Some domain.rank_domain_owner;
              }

let expect_sort expected term =
  if sort_equal expected term.term_sort then Ok ()
  else
    error term.term_span "expected %s term, found %s" (sort_to_string expected)
      (sort_to_string term.term_sort)

let binary_same ~span ~result ~node ~features left right =
  if not (sort_equal left.term_sort right.term_sort) then
    error span "operands have different sorts %s and %s"
      (sort_to_string left.term_sort) (sort_to_string right.term_sort)
  else make_term ~sort:result ~span ~node:(node left right) ~features [ left; right ]

let expect_bv term =
  match term.term_sort with
  | Bv width -> Ok width
  | Int | Bool | Named _ ->
      error term.term_span "expected BV term, found %s"
        (sort_to_string term.term_sort)

let bv_binary ~operation ~span ~result ~node left right =
  let result =
    match (expect_bv left, expect_bv right) with
    | Error _ as error, _ | _, (Error _ as error) -> error
    | Ok left_width, Ok right_width ->
        if not (Bv_width.equal left_width right_width) then
          error span "%s BV operands have different authenticated widths %s and %s"
            operation
            (Bv_width.to_string left_width) (Bv_width.to_string right_width)
        else
          make_term ~sort:(result left_width) ~span ~node:(node left right)
            ~features:(Feature_set.singleton Bit_vectors)
            [ left; right ]
  in
  [%log.trace "constructed typed closed-kernel BV binary node"
    ~stage:(Delator.Field.string "logic-ir-bv-construction")
    ~operation:(Delator.Field.string operation)
    ~accepted:(Delator.Field.bool (Result.is_ok result))
    ~decision:
      (Delator.Field.string (if Result.is_ok result then "accepted" else "rejected"))];
  result

let bv_unary ~span ~result ~node value =
  match expect_bv value with
  | Error _ as error -> error
  | Ok width ->
      make_term ~sort:(result width) ~span ~node:(node value)
        ~features:(Feature_set.singleton Bit_vectors)
        [ value ]

let bv_eq ~span left right =
  bv_binary ~operation:"eq" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_eq (left, right))
    left right

let bv_distinct ~span left right =
  bv_binary ~operation:"distinct" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_distinct (left, right))
    left right

let bv_add_mod ~span left right =
  bv_binary ~operation:"add-mod" ~span ~result:(fun width -> Bv width)
    ~node:(fun left right -> Bv_add_mod (left, right))
    left right

let bv_sub_mod ~span left right =
  bv_binary ~operation:"sub-mod" ~span ~result:(fun width -> Bv width)
    ~node:(fun left right -> Bv_sub_mod (left, right))
    left right

let bv_not ~span value =
  bv_unary ~span ~result:(fun width -> Bv width)
    ~node:(fun value -> Bv_not value) value

let bv_and ~span left right =
  bv_binary ~operation:"and" ~span ~result:(fun width -> Bv width)
    ~node:(fun left right -> Bv_and (left, right))
    left right

let bv_or ~span left right =
  bv_binary ~operation:"or" ~span ~result:(fun width -> Bv width)
    ~node:(fun left right -> Bv_or (left, right))
    left right

let bv_xor ~span left right =
  bv_binary ~operation:"xor" ~span ~result:(fun width -> Bv width)
    ~node:(fun left right -> Bv_xor (left, right))
    left right

let bv_ult ~span left right =
  bv_binary ~operation:"unsigned-less-than" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_ult (left, right))
    left right

let bv_ule ~span left right =
  bv_binary ~operation:"unsigned-less-or-equal" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_ule (left, right))
    left right

let bv_ugt ~span left right =
  bv_binary ~operation:"unsigned-greater-than" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_ugt (left, right))
    left right

let bv_uge ~span left right =
  bv_binary ~operation:"unsigned-greater-or-equal" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_uge (left, right))
    left right

let bv_slt ~span left right =
  bv_binary ~operation:"signed-less-than" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_slt (left, right))
    left right

let bv_sle ~span left right =
  bv_binary ~operation:"signed-less-or-equal" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_sle (left, right))
    left right

let bv_sgt ~span left right =
  bv_binary ~operation:"signed-greater-than" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_sgt (left, right))
    left right

let bv_sge ~span left right =
  bv_binary ~operation:"signed-greater-or-equal" ~span ~result:(fun _ -> Bool)
    ~node:(fun left right -> Bv_sge (left, right))
    left right

let bv_to_int_unsigned ~span value =
  bv_unary ~span ~result:(fun _ -> Int)
    ~node:(fun value -> Bv_to_int_unsigned value) value
  |> Result.map (fun term ->
         { term with
           term_features =
             Feature_set.add Int_bitvector_conversions term.term_features })

let bv_to_int_signed ~span value =
  bv_unary ~span ~result:(fun _ -> Int)
    ~node:(fun value -> Bv_to_int_signed value) value
  |> Result.map (fun term ->
         { term with
           term_features =
             Feature_set.add Int_bitvector_conversions term.term_features })

let int_to_bv_mod ~span ~width value =
  match (validate_sort ~span (Bv width), expect_sort Int value) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok (), Ok () ->
      make_term ~sort:(Bv width) ~span ~node:(Int_to_bv_mod (width, value))
        ~features:
          (Feature_set.of_list [ Bit_vectors; Int_bitvector_conversions ])
        [ value ]

let integer_binary ~span ~result ~node left right =
  match (expect_sort Int left, expect_sort Int right) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok (), Ok () ->
      make_term ~sort:result ~span ~node:(node left right)
        ~features:(Feature_set.singleton Linear_integer_arithmetic)
        [ left; right ]

let add ~span left right = integer_binary ~span ~result:Int ~node:(fun l r -> Add (l, r)) left right

let subtract ~span left right =
  integer_binary ~span ~result:Int ~node:(fun l r -> Subtract (l, r)) left right

let negate ~span value =
  match expect_sort Int value with
  | Error _ as error -> error
  | Ok () ->
      make_term ~sort:Int ~span ~node:(Negate value)
        ~features:(Feature_set.singleton Linear_integer_arithmetic)
        [ value ]

let multiply ~span left right =
  match (expect_sort Int left, expect_sort Int right) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok (), Ok () ->
      make_term ~sort:Int ~span ~node:(Multiply (left, right))
        ~features:(Feature_set.singleton Nonlinear_integer_arithmetic)
        [ left; right ]

let scale ~span coefficient value =
  match expect_sort Int value with
  | Error _ as error -> error
  | Ok () ->
      make_term ~sort:Int ~span ~node:(Scale (coefficient, value))
        ~features:(Feature_set.singleton Linear_integer_arithmetic)
        [ value ]

let less_than ~span left right =
  integer_binary ~span ~result:Bool
    ~node:(fun l r -> Less_than (l, r))
    left right

let less_or_equal ~span left right =
  integer_binary ~span ~result:Bool
    ~node:(fun l r -> Less_or_equal (l, r))
    left right

let greater_than ~span left right =
  integer_binary ~span ~result:Bool
    ~node:(fun l r -> Greater_than (l, r))
    left right

let greater_or_equal ~span left right =
  integer_binary ~span ~result:Bool
    ~node:(fun l r -> Greater_or_equal (l, r))
    left right

let equal ~span left right =
  match (left.term_sort, right.term_sort) with
  | Bv _, Bv _ -> bv_eq ~span left right
  | _ ->
      binary_same ~span ~result:Bool ~node:(fun l r -> Equal (l, r))
        ~features:Feature_set.empty left right

let distinct ~span left right =
  match (left.term_sort, right.term_sort) with
  | Bv _, Bv _ -> bv_distinct ~span left right
  | _ ->
      binary_same ~span ~result:Bool ~node:(fun l r -> Distinct (l, r))
        ~features:Feature_set.empty left right

let not_ ~span value =
  match expect_sort Bool value with
  | Error _ as error -> error
  | Ok () ->
      make_term ~sort:Bool ~span ~node:(Not value) ~features:Feature_set.empty
        [ value ]

let boolean_list ~span ~kind ~node values =
  if values = [] then error span "%s requires at least one operand" kind
  else
    match List.find_opt (fun value -> not (sort_equal Bool value.term_sort)) values with
    | Some value ->
        error value.term_span "%s expects Boolean operands, found %s" kind
          (sort_to_string value.term_sort)
    | None ->
        make_term ~sort:Bool ~span ~node:(node values) ~features:Feature_set.empty
          values

let and_ ~span values = boolean_list ~span ~kind:"and" ~node:(fun values -> And values) values
let or_ ~span values = boolean_list ~span ~kind:"or" ~node:(fun values -> Or values) values

let implies ~span premise consequence =
  match (expect_sort Bool premise, expect_sort Bool consequence) with
  | Error _ as error, _ | _, (Error _ as error) -> error
  | Ok (), Ok () ->
      make_term ~sort:Bool ~span ~node:(Implies (premise, consequence))
        ~features:Feature_set.empty [ premise; consequence ]

let ite ~span condition ~then_ ~else_ =
  match expect_sort Bool condition with
  | Error _ as error -> error
  | Ok () ->
      if not (sort_equal then_.term_sort else_.term_sort) then
        error span "ITE branches have different sorts %s and %s"
          (sort_to_string then_.term_sort) (sort_to_string else_.term_sort)
      else
        make_term ~sort:then_.term_sort ~span
          ~node:(Ite (condition, then_, else_))
          ~features:Feature_set.empty [ condition; then_; else_ ]

let term_is_pattern_head = function
  | { term_node = Apply (_, _ :: _); _ } -> true
  | { term_node = Rank_project (_, _, _, _); _ } -> true
  | _ -> false

let user_quantifier builder ~universal ~binders ~body ~trigger ~qid ~skid
    ~span =
  let owner_error term =
    match term.term_owner with
    | Some owner -> not (same_owner owner builder.owner)
    | None -> false
  in
  let trigger_terms = Option.to_list trigger in
  let rec duplicate indices names = function
    | [] -> None
    | binder :: rest ->
        if
          Int_set.mem binder.binder_index indices
          || List.mem binder.binder_name names
        then Some binder
        else
          duplicate
            (Int_set.add binder.binder_index indices)
            (binder.binder_name :: names) rest
  in
  if binders = [] then
    error span "user quantifier binder vector must be nonempty"
  else if
    List.exists
      (fun binder -> not (same_owner binder.binder_owner builder.owner))
      binders
  then
    error span "user quantifier uses a binder from another logic signature"
  else if Option.is_some (duplicate Int_set.empty [] binders) then
    error span "user quantifier binder vector contains a duplicate"
  else if not (sort_equal Bool body.term_sort) then
    error body.term_span "user quantifier body must be Boolean, found %s"
      (sort_to_string body.term_sort)
  else if owner_error body || List.exists owner_error trigger_terms then
    error span "user quantifier contains a term from another logic signature"
  else if not (valid_name qid) || not (valid_name skid) then
    error span "user quantifier qid/skid must be nonempty and portable"
  else
    match (universal, trigger) with
    | true, None ->
        error span "universal user quantifier requires one explicit trigger"
    | false, Some trigger ->
        error trigger.term_span
          "existential user quantifier must not carry a trigger"
    | true, Some trigger when not (term_is_pattern_head trigger) ->
        error trigger.term_span
          "universal user quantifier trigger must be application-headed"
    | true, Some trigger
      when
        not
          (List.for_all
             (fun binder ->
               Int_set.mem binder.binder_index trigger.free_binders)
             binders) ->
        error trigger.term_span
          "universal user quantifier trigger does not cover every binder"
    | (true, Some _ | false, None) ->
        let terms = body :: trigger_terms in
        let free_binders =
          List.fold_left
            (fun free binder -> Int_set.remove binder.binder_index free)
            (merge_sets (fun term -> term.free_binders) terms)
            binders
        in
        let features =
          Feature_set.of_list
            (if universal then
               [ Quantifiers; Explicit_patterns; Quantifier_ids ]
             else [ Quantifiers; Quantifier_ids ])
          |> Feature_set.union (merge_features terms)
          |> Feature_set.union (binder_sort_features binders)
        in
        Ok
          {
            term_sort = Bool;
            term_span = span;
            term_node =
              (let quantifier =
                 {
                   user_quantifier_binders = binders;
                   user_quantifier_body = body;
                   user_quantifier_trigger = trigger;
                   user_quantifier_qid = qid;
                   user_quantifier_skid = skid;
                   user_quantifier_span = span;
                 }
               in
               if universal then Forall_term quantifier
               else Exists_term quantifier);
            free_binders;
            used_functions =
              merge_sets (fun term -> term.used_functions) terms;
            term_features = features;
            term_owner = Some builder.owner;
          }

let forall_term builder ~binders ~body ~trigger ~qid ~skid ~span =
  user_quantifier builder ~universal:true ~binders ~body
    ~trigger:(Some trigger) ~qid ~skid ~span

let exists_term builder ~binders ~body ~qid ~skid ~span =
  user_quantifier builder ~universal:false ~binders ~body ~trigger:None ~qid
    ~skid ~span

let duplicate_binder binders =
  let rec loop ids names = function
    | [] -> None
    | binder :: rest ->
        if
          Int_set.mem binder.binder_index ids
          || List.mem binder.binder_name names
        then Some binder
        else
          loop
            (Int_set.add binder.binder_index ids)
            (binder.binder_name :: names) rest
  in
  loop Int_set.empty [] binders

let binder_ids binders =
  List.fold_left
    (fun ids binder -> Int_set.add binder.binder_index ids)
    Int_set.empty binders

let forall builder ~binders ~body ~patterns ~qid ~skid ~span =
  if binders = [] then error span "universal axiom requires at least one binder"
  else
    match duplicate_binder binders with
    | Some binder ->
        error binder.binder_span
          "quantifier binders must have unique identities and names"
    | None ->
        if
          List.exists
            (fun binder -> not (same_owner builder.owner binder.binder_owner))
            binders
        then error span "quantifier uses a binder from another logic signature"
        else if not (sort_equal Bool body.term_sort) then
          error body.term_span "quantifier body must be Boolean, found %s"
            (sort_to_string body.term_sort)
        else if patterns = [] then
          error span "quantifier requires at least one explicit pattern"
        else if not (valid_name qid) then
          error span "quantifier qid must be nonempty and contain no control characters"
        else if not (valid_name skid) then
          error span "quantifier skid must be nonempty and contain no control characters"
        else
          let bound_ids = binder_ids binders in
          let owner_error term =
            match term.term_owner with
            | Some owner when not (same_owner owner builder.owner) -> true
            | Some _ | None -> false
          in
          if owner_error body then
            error body.term_span
              "quantifier body uses declarations from another logic signature"
          else if not (Int_set.subset body.free_binders bound_ids) then
            error body.term_span "quantifier body contains an unbound binder"
          else
            let validate_pattern pattern =
              if pattern = [] then error span "quantifier pattern cannot be empty"
              else
                match
                  List.find_opt
                    (fun term ->
                      owner_error term
                      || not (Int_set.subset term.free_binders bound_ids))
                    pattern
                with
                | Some term ->
                    error term.term_span
                      "quantifier pattern contains an unbound or foreign binder"
                | None -> (
                    match List.find_opt (fun term -> not (term_is_pattern_head term)) pattern with
                    | Some term ->
                        error term.term_span
                          "quantifier pattern terms must be nonconstant function applications"
                    | None ->
                        let covered =
                          merge_sets (fun term -> term.free_binders) pattern
                        in
                        if Int_set.equal covered bound_ids then Ok ()
                        else
                          error span
                            "each explicit pattern must cover every quantifier binder")
            in
            let rec validate_patterns = function
              | [] -> Ok ()
              | pattern :: rest -> (
                  match validate_pattern pattern with
                  | Error _ as error -> error
                  | Ok () -> validate_patterns rest)
            in
            (match validate_patterns patterns with
            | Error _ as error -> error
            | Ok () ->
                let pattern_terms = List.concat patterns in
                let features =
                  Feature_set.of_list
                    [ Quantifiers; Explicit_patterns; Quantifier_ids ]
                  |> Feature_set.union body.term_features
                  |> Feature_set.union (merge_features pattern_terms)
                  |> Feature_set.union (binder_sort_features binders)
                in
                Ok
                  {
                    axiom_owner = builder.owner;
                    axiom_binders = binders;
                    axiom_body = body;
                    axiom_patterns = patterns;
                    axiom_qid = qid;
                    axiom_skid = skid;
                    axiom_span = span;
                    axiom_features = features;
                  })

let declaration_features = function
  | Sort_declaration _ -> Feature_set.singleton Named_sorts
  | Function_declaration function_ ->
      List.fold_left
        (fun features sort ->
          let features = Feature_set.union features (sort_features sort) in
          match sort with
          | Named _ -> Feature_set.add Named_sorts features
          | Int | Bool | Bv _ -> features)
        (Feature_set.singleton Uninterpreted_functions)
        (function_.function_range :: function_.function_domain)

let project_bv builder ~identity term ~span =
  match validate_name ~kind:"BV projection" ~span identity with
  | Error _ as error -> error
  | Ok () -> (
      match expect_bv term with
      | Error _ as error -> error
      | Ok width -> (
          match validate_sort ~span (Bv width) with
          | Error _ as error -> error
          | Ok () ->
          if not (Int_set.is_empty term.free_binders) then
            error span "BV projection %S contains an unbound binder" identity
          else
            match term.term_owner with
            | Some owner when not (same_owner owner builder.owner) ->
                error span "BV projection %S belongs to another logic signature"
                  identity
            | Some _ | None ->
                Ok
                  { bv_projection_owner = builder.owner;
                    bv_projection_identity = identity;
                    bv_projection_width = width;
                    bv_projection_term = term;
                    bv_projection_span = span }))

let query ?(bv_projections = []) builder ~axioms ~assertions ~requires ~span =
  let declarations = List.rev builder.declarations_reversed in
  let datatypes = List.rev builder.datatypes_reversed in
  let projection_identities =
    List.map (fun projection -> projection.bv_projection_identity) bv_projections
  in
  match
    List.find_opt
      (fun projection ->
        not (same_owner builder.owner projection.bv_projection_owner))
      bv_projections
  with
  | Some projection ->
      error projection.bv_projection_span
        "query contains a BV projection from another logic signature"
  | None when
      List.length projection_identities
      <> List.length (List.sort_uniq String.compare projection_identities) ->
      error span "query contains duplicate BV projection identities"
  | None ->
  match
    List.find_opt
      (fun axiom -> not (same_owner builder.owner axiom.axiom_owner))
      axioms
  with
  | Some axiom ->
      error axiom.axiom_span "query contains an axiom from another logic signature"
  | None -> (
      match
        List.find_opt
          (fun assertion ->
            match assertion.term_owner with
            | Some owner -> not (same_owner owner builder.owner)
            | None -> false)
          assertions
      with
      | Some assertion ->
          error assertion.term_span
            "query contains a term from another logic signature"
      | None -> (
          match
            List.find_opt
              (fun assertion ->
                not (sort_equal Bool assertion.term_sort)
                || not (Int_set.is_empty assertion.free_binders))
              assertions
          with
          | Some assertion when not (sort_equal Bool assertion.term_sort) ->
              error assertion.term_span "query assertion must be Boolean, found %s"
                (sort_to_string assertion.term_sort)
          | Some assertion ->
              error assertion.term_span "query assertion contains an unbound binder"
          | None ->
              let declaration_ids =
                List.fold_left
                  (fun ids -> function
                    | Sort_declaration _ -> ids
                    | Function_declaration function_ ->
                        Int_set.add function_.function_index ids)
                  Int_set.empty declarations
              in
              let all_term_functions =
                List.fold_left
                  (fun ids assertion ->
                    Int_set.union ids assertion.used_functions)
                  Int_set.empty assertions
                |> fun ids ->
                List.fold_left
                  (fun ids axiom ->
                    let ids = Int_set.union ids axiom.axiom_body.used_functions in
                    List.fold_left
                      (fun ids pattern ->
                        List.fold_left
                          (fun ids term ->
                            Int_set.union ids term.used_functions)
                          ids pattern)
                      ids axiom.axiom_patterns)
                  ids axioms
                |> fun ids ->
                List.fold_left
                  (fun ids projection ->
                    Int_set.union ids
                      projection.bv_projection_term.used_functions)
                  ids bv_projections
              in
              if not (Int_set.subset all_term_functions declaration_ids) then
                error span "query references a function absent from its declaration order"
              else
                let features =
                  List.fold_left
                    (fun features declaration ->
                      Feature_set.union features
                        (declaration_features declaration))
                    (Feature_set.of_list requires) declarations
                  |> fun features ->
                  (if datatypes = [] then features
                   else
                     Feature_set.add Algebraic_datatypes
                       (Feature_set.add Named_sorts features))
                  |> fun features ->
                  List.fold_left
                    (fun features axiom ->
                      Feature_set.union features axiom.axiom_features)
                    features axioms
                  |> fun features ->
                  List.fold_left
                    (fun features assertion ->
                      Feature_set.union features assertion.term_features)
                    features assertions
                  |> fun features ->
                  List.fold_left
                    (fun features projection ->
                      Feature_set.union features
                        projection.bv_projection_term.term_features)
                    features bv_projections
                  |> fun features ->
                  if bv_projections = [] then features
                  else Feature_set.add Models features
                in
                Ok
                  {
                    query_declarations = declarations;
                    query_datatypes = datatypes;
                    query_axioms = axioms;
                    query_assertions = assertions;
                    query_bv_projections = bv_projections;
                    query_requirements = Feature_set.elements features;
                    query_span = span;
                  }))

let term_sort term = term.term_sort
let term_span term = term.term_span
let requirements query = query.query_requirements

let feature_to_string = function
  | Named_sorts -> "named-sorts"
  | Uninterpreted_functions -> "uninterpreted-functions"
  | Linear_integer_arithmetic -> "linear-integer-arithmetic"
  | Quantifiers -> "quantifiers"
  | Explicit_patterns -> "explicit-patterns"
  | Quantifier_ids -> "quantifier-ids"
  | Models -> "models"
  | Nonlinear_integer_arithmetic -> "nonlinear-integer-arithmetic"
  | Algebraic_datatypes -> "algebraic-datatypes"
  | Bit_vectors -> "bit-vectors"
  | Int_bitvector_conversions -> "int-bitvector-conversions"

let error_to_string error = error.message

module View = struct
  type nonrec declaration = declaration =
    | Sort_declaration of named_sort
    | Function_declaration of function_symbol

  type nonrec term_node = term_node =
    | Integer of Z.t
    | Boolean of bool
    | Bound of binder
    | Apply of function_symbol * term list
    | Rank_project of rank_domain * string * function_symbol * term
    | Add of term * term
    | Subtract of term * term
    | Negate of term
    | Multiply of term * term
    | Scale of Z.t * term
    | Less_than of term * term
    | Less_or_equal of term * term
    | Greater_than of term * term
    | Greater_or_equal of term * term
    | Equal of term * term
    | Distinct of term * term
    | Not of term
    | And of term list
    | Or of term list
    | Implies of term * term
    | Forall_term of user_quantifier
    | Exists_term of user_quantifier
    | Ite of term * term * term
    | Bv_literal of Bv_value.t
    | Bv_eq of term * term
    | Bv_distinct of term * term
    | Bv_add_mod of term * term
    | Bv_sub_mod of term * term
    | Bv_not of term
    | Bv_and of term * term
    | Bv_or of term * term
    | Bv_xor of term * term
    | Bv_ult of term * term
    | Bv_ule of term * term
    | Bv_ugt of term * term
    | Bv_uge of term * term
    | Bv_slt of term * term
    | Bv_sle of term * term
    | Bv_sgt of term * term
    | Bv_sge of term * term
    | Bv_to_int_unsigned of term
    | Bv_to_int_signed of term
    | Int_to_bv_mod of Bv_width.t * term

  let declarations query = query.query_declarations
  let datatypes query = query.query_datatypes
  let axioms query = query.query_axioms
  let assertions query = query.query_assertions
  let bv_projections query = query.query_bv_projections
  let query_span query = query.query_span
  let bv_projection_identity projection = projection.bv_projection_identity
  let bv_projection_width projection = projection.bv_projection_width
  let bv_projection_term projection = projection.bv_projection_term
  let bv_projection_span projection = projection.bv_projection_span
  let named_sort_index sort = sort.named_sort_index
  let named_sort_name sort = sort.named_sort_name
  let named_sort_span sort = sort.named_sort_span
  let function_index function_ = function_.function_index
  let function_name function_ = function_.function_name
  let function_domain function_ = function_.function_domain
  let function_range function_ = function_.function_range
  let function_span function_ = function_.function_span
  let datatype_id datatype = datatype.datatype_id
  let datatype_scc_id datatype = datatype.datatype_scc_id
  let datatype_sort datatype = datatype.datatype_sort
  let datatype_constructors datatype = datatype.datatype_constructors
  let datatype_constructor_id constructor =
    constructor.datatype_constructor_id
  let datatype_constructor_symbol constructor =
    constructor.datatype_constructor_symbol
  let datatype_recognizer_symbol constructor =
    constructor.datatype_recognizer_symbol
  let datatype_fields constructor = constructor.datatype_fields
  let datatype_field_id field = field.datatype_field_id
  let datatype_field_sort field = field.datatype_field_sort
  let datatype_field_symbol field = field.datatype_field_symbol
  let rank_domain_id domain = domain.rank_domain_id

  let rank_domain_members domain =
    List.map
      (fun (member, sort, _) -> (member, sort))
      domain.rank_domain_members

  let rank_domain_span domain = domain.rank_domain_span
  let binder_index binder = binder.binder_index
  let binder_name binder = binder.binder_name
  let binder_sort binder = binder.binder_sort
  let binder_span binder = binder.binder_span
  let term_node term = term.term_node
  let user_quantifier_binders quantifier = quantifier.user_quantifier_binders
  let user_quantifier_body quantifier = quantifier.user_quantifier_body
  let user_quantifier_trigger quantifier = quantifier.user_quantifier_trigger
  let user_quantifier_qid quantifier = quantifier.user_quantifier_qid
  let user_quantifier_skid quantifier = quantifier.user_quantifier_skid
  let user_quantifier_span quantifier = quantifier.user_quantifier_span
  let axiom_binders axiom = axiom.axiom_binders
  let axiom_body axiom = axiom.axiom_body
  let axiom_patterns axiom = axiom.axiom_patterns
  let axiom_qid axiom = axiom.axiom_qid
  let axiom_skid axiom = axiom.axiom_skid
  let axiom_span axiom = axiom.axiom_span
end
