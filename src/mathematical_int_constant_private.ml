type multiplication =
  | Constant_pair of Z.t * Z.t
  | Constant_left of Z.t
  | Constant_right of Z.t
  | Nonlinear

type literal_shape =
  | Decimal_digits of int
  | Prefixed_digits of { radix : string; payload_start : int }

let parse_literal (decoded [@delator.skip]) =
  let length = String.length decoded in
  let sign_end =
    if length > 0 && (Char.equal decoded.[0] '-' || Char.equal decoded.[0] '+')
    then 1
    else 0
  in
  let shape =
    if sign_end + 1 < length && Char.equal decoded.[sign_end] '0' then
      match decoded.[sign_end + 1] with
      | 'x' | 'X' ->
          Prefixed_digits
            { radix = "hexadecimal"; payload_start = sign_end + 2 }
      | 'o' | 'O' ->
          Prefixed_digits { radix = "octal"; payload_start = sign_end + 2 }
      | 'b' | 'B' ->
          Prefixed_digits { radix = "binary"; payload_start = sign_end + 2 }
      | _ -> Decimal_digits sign_end
    else Decimal_digits sign_end
  in
  let payload_start =
    match shape with
    | Decimal_digits start -> start
    | Prefixed_digits { payload_start; _ } -> payload_start
  in
  let result =
    if length = 0 || payload_start >= length then None
    else
      try Some (Z.of_string decoded) with Invalid_argument _ -> None
  in
  (match result with
  | Some _ ->
      [%log.trace "accepted mathematical integer literal"
        ~stage:(Delator.Field.string "mathematical-int-constant")
        ~source_class:(Delator.Field.string "decoded-string-literal")
        ~radix:
          (Delator.Field.string
             (match shape with
             | Decimal_digits _ -> "decimal"
             | Prefixed_digits { radix; _ } -> radix))
        ~decoded_length:(Delator.Field.int (String.length decoded))
        ~negative:
          (Delator.Field.bool
             (String.length decoded > 0 && Char.equal decoded.[0] '-'))
        ~decision:(Delator.Field.string "accepted")]
  | None ->
      [%log.trace "rejected malformed mathematical integer literal"
        ~stage:(Delator.Field.string "mathematical-int-constant")
        ~source_class:(Delator.Field.string "decoded-string-literal")
        ~decoded_length:(Delator.Field.int (String.length decoded))
        ~decision:(Delator.Field.string "rejected")
        ~reason_class:(Delator.Field.string "integer-literal-grammar")]);
  result
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]

let classify_multiplication ~coefficient ~left:(left [@delator.skip])
    ~right:(right [@delator.skip]) =
  let result =
    match (coefficient left, coefficient right) with
    | Some left, Some right -> Constant_pair (left, right)
    | Some constant, None -> Constant_left constant
    | None, Some constant -> Constant_right constant
    | None, None -> Nonlinear
  in
  [%log.trace "classified integer multiplication coefficient shape"
    ~stage:(Delator.Field.string "mathematical-int-constant")
    ~coefficient_shape:
      (Delator.Field.string
         (match result with
         | Constant_pair _ -> "both"
         | Constant_left _ -> "left"
         | Constant_right _ -> "right"
         | Nonlinear -> "none"))
    ~decision:
      (Delator.Field.string
         (match result with Nonlinear -> "general-term" | _ -> "constant-form"))];
  result
[@@delator.instrument] [@@delator.level trace] [@@delator.no_exn_log]
