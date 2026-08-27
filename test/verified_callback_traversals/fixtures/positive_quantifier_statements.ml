type 'a box = Box of 'a
type 'a sequence = Empty | Item of 'a * 'a sequence
type ('a, 'b) pair = { left : 'a; right : 'b }

[%%verocaml.symbolic val integer_image : int -> int]
[%%verocaml.symbolic val boolean_image : bool -> bool]
[%%verocaml.symbolic val parametric_image : 'a -> 'a]
[%%verocaml.symbolic val option_image : 'a option -> 'a option]
[%%verocaml.symbolic val box_image : 'a box -> 'a box]
[%%verocaml.symbolic val sequence_image : 'a sequence -> 'a sequence]
[%%verocaml.symbolic val pair_image : ('a, 'b) pair -> ('a, 'b) pair]

let identity (value : 'a) : 'a = value [@@verocaml.spec]

let proof_forall_integer () : unit =
  [%verocaml.assert
    forall (fun (candidate : int) ->
      ((integer_image candidate) [@trigger]) = integer_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_boolean (_argument : unit) : unit =
  [%verocaml.assert
    forall (fun (candidate : bool) ->
      ((boolean_image candidate) [@trigger]) = boolean_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_parametric (_value : 'a) : unit =
  [%verocaml.assert
    forall (fun (candidate : 'a) ->
      ((parametric_image candidate) [@trigger]) = parametric_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_option (_value : 'a option) : unit =
  [%verocaml.assert
    forall (fun (candidate : 'a option) ->
      ((option_image candidate) [@trigger]) = option_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_box (_value : 'a box) : unit =
  [%verocaml.assert
    forall (fun (candidate : 'a box) ->
      ((box_image candidate) [@trigger]) = box_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_sequence (_value : 'a sequence) : unit =
  [%verocaml.assert
    forall (fun (candidate : 'a sequence) ->
      ((sequence_image candidate) [@trigger]) = sequence_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_pair (_value : ('a, 'b) pair) : unit =
  [%verocaml.assert
    forall (fun (candidate : ('a, 'b) pair) ->
      ((pair_image candidate) [@trigger]) = pair_image candidate)];
  ()
[@@verocaml.proof]

let proof_forall_function (value : int) : unit =
  [%verocaml.assert
    forall (fun (f : int -> int) -> (f value [@trigger]) = f value)];
  ()
[@@verocaml.proof]

let proof_exists_integer () : unit =
  [%verocaml.assert exists (fun (candidate : int) -> candidate = 0)];
  ()
[@@verocaml.proof]

let proof_exists_boolean (value : bool) : unit =
  [%verocaml.assert exists (fun (candidate : bool) -> candidate = value)];
  ()
[@@verocaml.proof]

let proof_exists_parametric (value : 'a) : unit =
  [%verocaml.assert exists (fun (candidate : 'a) -> candidate = value)];
  ()
[@@verocaml.proof]

let proof_exists_option (value : 'a option) : unit =
  [%verocaml.assert
    exists (fun (candidate : 'a option) -> candidate = value)];
  ()
[@@verocaml.proof]

let proof_exists_box (value : 'a box) : unit =
  [%verocaml.assert exists (fun (candidate : 'a box) -> candidate = value)];
  ()
[@@verocaml.proof]

let proof_exists_sequence (value : 'a sequence) : unit =
  [%verocaml.assert
    exists (fun (candidate : 'a sequence) -> candidate = value)];
  ()
[@@verocaml.proof]

let proof_exists_pair (value : ('a, 'b) pair) : unit =
  [%verocaml.assert
    exists (fun (candidate : ('a, 'b) pair) -> candidate = value)];
  ()
[@@verocaml.proof]

let proof_exists_function (value : int) : unit =
  [%verocaml.assert
    exists (fun (f : int -> int) -> f value = f value)];
  ()
[@@verocaml.proof]

let assert_forall_integer (value : int) : int =
  [%verocaml.assert
    forall (fun (candidate : int) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_boolean (value : bool) : bool =
  [%verocaml.assert
    forall (fun (candidate : bool) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_parametric (value : 'a) : 'a =
  [%verocaml.assert
    forall (fun (candidate : 'a) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_option (value : 'a option) : 'a option =
  [%verocaml.assert
    forall (fun (candidate : 'a option) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_box (value : 'a box) : 'a box =
  [%verocaml.assert
    forall (fun (candidate : 'a box) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_sequence (value : 'a sequence) : 'a sequence =
  [%verocaml.assert
    forall (fun (candidate : 'a sequence) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_pair (value : ('a, 'b) pair) : ('a, 'b) pair =
  [%verocaml.assert
    forall (fun (candidate : ('a, 'b) pair) ->
      ((identity candidate) [@trigger]) = candidate)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_forall_function (value : int) : int =
  [%verocaml.assert
    forall (fun (f : int -> int) -> (f value [@trigger]) = f value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_integer (value : int) : int =
  [%verocaml.assert
    exists (fun (candidate : int) ->
      candidate = value && integer_image candidate = integer_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_boolean (value : bool) : bool =
  [%verocaml.assert
    exists (fun (candidate : bool) ->
      candidate = value && boolean_image candidate = boolean_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_parametric (value : 'a) : 'a =
  [%verocaml.assert
    exists (fun (candidate : 'a) ->
      candidate = value
      && parametric_image candidate = parametric_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_option (value : 'a option) : 'a option =
  [%verocaml.assert
    exists (fun (candidate : 'a option) ->
      candidate = value && option_image candidate = option_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_box (value : 'a box) : 'a box =
  [%verocaml.assert
    exists (fun (candidate : 'a box) ->
      candidate = value && box_image candidate = box_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_sequence (value : 'a sequence) : 'a sequence =
  [%verocaml.assert
    exists (fun (candidate : 'a sequence) ->
      candidate = value
      && sequence_image candidate = sequence_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_pair (value : ('a, 'b) pair) : ('a, 'b) pair =
  [%verocaml.assert
    exists (fun (candidate : ('a, 'b) pair) ->
      candidate = value && pair_image candidate = pair_image value)];
  [%verocaml.ensures fun result -> result = value];
  value

let assert_exists_function (value : int) : int =
  [%verocaml.assert
    exists (fun (f : int -> int) -> f value = f value)];
  [%verocaml.ensures fun result -> result = value];
  value
