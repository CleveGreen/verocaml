[@@@verocaml.activate [group_seq_axioms]]

let integer_sequence size offset =
  init size (fun index -> offset + index)
[@@verocaml.spec]

let boolean_index_sequence size selected =
  init size (fun index -> index = selected)
[@@verocaml.spec]

let constant_boolean_sequence size value =
  init size (fun _index -> value)
[@@verocaml.spec]

let logical_push sequence value = push sequence value
[@@verocaml.spec]

let logical_update sequence index value = update sequence index value
[@@verocaml.spec]

let logical_subrange sequence lower upper =
  subrange sequence lower upper
[@@verocaml.spec]

let logical_append left right = append left right
[@@verocaml.spec]

let pushed_integer_sequence final_value =
  logical_push (logical_push (integer_sequence 2 1) 7) final_value
[@@verocaml.spec]

let branching_sequence flag =
  if flag then logical_push (singleton false) true
  else logical_push (singleton true) false
[@@verocaml.spec]

let updated_integer_sequence value =
  logical_update (integer_sequence 5 0) 2 value
[@@verocaml.spec]

let updated_boolean_sequence value =
  logical_update (constant_boolean_sequence 3 false) 1 value
[@@verocaml.spec]

let polymorphic_updated_sequence old_value new_value =
  logical_update
    (logical_push (singleton old_value) old_value)
    0 new_value
[@@verocaml.spec]

let middle_integer_sequence offset =
  logical_subrange (integer_sequence 6 offset) 2 5
[@@verocaml.spec]

let appended_integer_sequence left_offset right_offset =
  logical_append
    (integer_sequence 2 left_offset)
    (integer_sequence 3 right_offset)
[@@verocaml.spec]

let polymorphic_appended_sequence left_value right_value =
  logical_append (singleton left_value) (singleton right_value)
[@@verocaml.spec]

let composed_changed_sequence flag =
  if flag then logical_update (integer_sequence 4 1) 1 20
  else logical_update (integer_sequence 4 1) 2 30
[@@verocaml.spec]

let composed_framed_sequence flag =
  logical_append
    (singleton 0)
    (logical_push (composed_changed_sequence flag) 5)
[@@verocaml.spec]

let composed_middle_sequence flag =
  logical_subrange (composed_framed_sequence flag) 1 5
[@@verocaml.spec]

let empty_int_observations () : unit =
  assert(length (empty : int t) = 0);
  [%verocaml.assert is_empty (empty : int t)];
  [%verocaml.assert not (valid_index (empty : int t) 0)];
  [%verocaml.assert valid_subrange (empty : int t) 0 0];
  [%verocaml.assert not (contains (empty : int t) 7)];
  ()
[@@verocaml.proof]

let empty_bool_observations () : unit =
  [%verocaml.assert length (empty : bool t) = 0];
  [%verocaml.assert is_empty (empty : bool t)];
  [%verocaml.assert not (valid_index (empty : bool t) (-1))];
  [%verocaml.assert not (contains (empty : bool t) true)];
  ()
[@@verocaml.proof]

let initialized_integers () : unit =
  [%verocaml.assert length (integer_sequence 4 10) = 4];
  [%verocaml.assert valid_index (integer_sequence 4 10) 0];
  [%verocaml.assert valid_index (integer_sequence 4 10) 3];
  [%verocaml.assert not (valid_index (integer_sequence 4 10) 4)];
  [%verocaml.assert valid_subrange (integer_sequence 4 10) 1 4];
  [%verocaml.assert get (integer_sequence 4 10) 0 = 10];
  [%verocaml.assert get (integer_sequence 4 10) 2 = 12];
  [%verocaml.assert first (integer_sequence 4 10) = 10];
  [%verocaml.assert last (integer_sequence 4 10) = 13];
  [%verocaml.assert contains (integer_sequence 4 10) 12];
  ()
[@@verocaml.proof]

let initialized_booleans () : unit =
  [%verocaml.assert length (boolean_index_sequence 3 1) = 3];
  [%verocaml.assert get (boolean_index_sequence 3 1) 0 = false];
  [%verocaml.assert get (boolean_index_sequence 3 1) 1 = true];
  [%verocaml.assert get (boolean_index_sequence 3 1) 2 = false];
  [%verocaml.assert first (boolean_index_sequence 3 1) = false];
  [%verocaml.assert last (boolean_index_sequence 3 1) = false];
  [%verocaml.assert contains (boolean_index_sequence 3 1) true];
  [%verocaml.assert contains (boolean_index_sequence 3 1) false];
  ()
[@@verocaml.proof]

let polymorphic_singleton (value : 'a) : unit =
  [%verocaml.assert length (singleton value) = 1];
  [%verocaml.assert valid_index (singleton value) 0];
  [%verocaml.assert get (singleton value) 0 = value];
  [%verocaml.assert first (singleton value) = value];
  [%verocaml.assert last (singleton value) = value];
  [%verocaml.assert contains (singleton value) value];
  ()
[@@verocaml.proof]

let mathematical_push_has_no_capacity_guard
    (sequence : 'a t)
    (value : 'a) : unit =
  [%verocaml.assert
    length (push sequence value) = length sequence + 1];
  ()
[@@verocaml.proof]

let mathematical_append_has_no_capacity_guard
    (left : 'a t)
    (right : 'a t) : unit =
  [%verocaml.assert
    length (append left right) = length left + length right];
  ()
[@@verocaml.proof]

let pushed_integer_chain () : unit =
  [%verocaml.assert
    length (logical_push (integer_sequence 2 1) 7) = 3];
  [%verocaml.assert length (pushed_integer_sequence 9) = 4];
  [%verocaml.assert get (pushed_integer_sequence 9) 0 = 1];
  [%verocaml.assert get (pushed_integer_sequence 9) 1 = 2];
  [%verocaml.assert get (pushed_integer_sequence 9) 2 = 7];
  [%verocaml.assert get (pushed_integer_sequence 9) 3 = 9];
  [%verocaml.assert first (pushed_integer_sequence 9) = 1];
  [%verocaml.assert last (pushed_integer_sequence 9) = 9];
  ()
[@@verocaml.proof]

let branching_push (flag : bool) : unit =
  [%verocaml.assert length (branching_sequence flag) = 2];
  [%verocaml.assert first (branching_sequence flag) = not flag];
  [%verocaml.assert last (branching_sequence flag) = flag];
  [%verocaml.assert contains (branching_sequence flag) flag];
  [%verocaml.assert contains (branching_sequence flag) (not flag)];
  ()
[@@verocaml.proof]

let updated_integers () : unit =
  [%verocaml.assert length (updated_integer_sequence 99) = 5];
  [%verocaml.assert get (updated_integer_sequence 99) 0 = 0];
  [%verocaml.assert get (updated_integer_sequence 99) 1 = 1];
  [%verocaml.assert get (updated_integer_sequence 99) 2 = 99];
  [%verocaml.assert get (updated_integer_sequence 99) 3 = 3];
  [%verocaml.assert get (updated_integer_sequence 99) 4 = 4];
  [%verocaml.assert first (updated_integer_sequence 99) = 0];
  [%verocaml.assert last (updated_integer_sequence 99) = 4];
  [%verocaml.assert contains (updated_integer_sequence 99) 99];
  ()
[@@verocaml.proof]

let updated_booleans () : unit =
  [%verocaml.assert length (updated_boolean_sequence true) = 3];
  [%verocaml.assert get (updated_boolean_sequence true) 0 = false];
  [%verocaml.assert get (updated_boolean_sequence true) 1 = true];
  [%verocaml.assert get (updated_boolean_sequence true) 2 = false];
  [%verocaml.assert contains (updated_boolean_sequence true) true];
  ()
[@@verocaml.proof]

let polymorphic_update (old_value : 'a) (new_value : 'a) : unit =
  [%verocaml.assert
    length (polymorphic_updated_sequence old_value new_value) = 2];
  [%verocaml.assert
    get (polymorphic_updated_sequence old_value new_value) 0 = new_value];
  [%verocaml.assert
    get (polymorphic_updated_sequence old_value new_value) 1 = old_value];
  [%verocaml.assert
    contains (polymorphic_updated_sequence old_value new_value) new_value];
  [%verocaml.assert
    contains (polymorphic_updated_sequence old_value new_value) old_value];
  ()
[@@verocaml.proof]

let direct_subrange () : unit =
  [%verocaml.assert length (middle_integer_sequence 10) = 3];
  [%verocaml.assert get (middle_integer_sequence 10) 0 = 12];
  [%verocaml.assert get (middle_integer_sequence 10) 1 = 13];
  [%verocaml.assert get (middle_integer_sequence 10) 2 = 14];
  [%verocaml.assert first (middle_integer_sequence 10) = 12];
  [%verocaml.assert last (middle_integer_sequence 10) = 14];
  [%verocaml.assert contains (middle_integer_sequence 10) 13];
  ()
[@@verocaml.proof]

let take_drop_skip () : unit =
  [%verocaml.assert length (take (integer_sequence 6 10) 3) = 3];
  [%verocaml.assert get (take (integer_sequence 6 10) 3) 0 = 10];
  [%verocaml.assert get (take (integer_sequence 6 10) 3) 2 = 12];
  [%verocaml.assert length (drop (integer_sequence 6 10) 3) = 3];
  [%verocaml.assert get (drop (integer_sequence 6 10) 3) 0 = 13];
  [%verocaml.assert get (drop (integer_sequence 6 10) 3) 2 = 15];
  [%verocaml.assert length (skip (integer_sequence 6 10) 4) = 2];
  [%verocaml.assert first (skip (integer_sequence 6 10) 4) = 14];
  [%verocaml.assert last (skip (integer_sequence 6 10) 4) = 15];
  ()
[@@verocaml.proof]

let appended_integers () : unit =
  [%verocaml.assert length (appended_integer_sequence 1 10) = 5];
  [%verocaml.assert get (appended_integer_sequence 1 10) 0 = 1];
  [%verocaml.assert get (appended_integer_sequence 1 10) 1 = 2];
  [%verocaml.assert get (appended_integer_sequence 1 10) 2 = 10];
  [%verocaml.assert get (appended_integer_sequence 1 10) 3 = 11];
  [%verocaml.assert get (appended_integer_sequence 1 10) 4 = 12];
  [%verocaml.assert first (appended_integer_sequence 1 10) = 1];
  [%verocaml.assert last (appended_integer_sequence 1 10) = 12];
  [%verocaml.assert contains (appended_integer_sequence 1 10) 11];
  ()
[@@verocaml.proof]

let polymorphic_append (left_value : 'a) (right_value : 'a) : unit =
  [%verocaml.assert
    length (polymorphic_appended_sequence left_value right_value) = 2];
  [%verocaml.assert
    get (polymorphic_appended_sequence left_value right_value) 0 = left_value];
  [%verocaml.assert
    get (polymorphic_appended_sequence left_value right_value) 1 = right_value];
  [%verocaml.assert
    first (polymorphic_appended_sequence left_value right_value) = left_value];
  [%verocaml.assert
    last (polymorphic_appended_sequence left_value right_value) = right_value];
  [%verocaml.assert
    contains (polymorphic_appended_sequence left_value right_value) left_value];
  [%verocaml.assert
    contains (polymorphic_appended_sequence left_value right_value) right_value];
  ()
[@@verocaml.proof]

let contains_singleton_exact (value : 'a) (other : 'a) : unit =
  [%verocaml.requires value <> other];
  [%verocaml.assert contains (singleton value) value];
  [%verocaml.assert not (contains (singleton value) other)];
  ()
[@@verocaml.proof]

let explicit_update_extensionality () : unit =
  axiom_extensionality
    (logical_update
       (integer_sequence 3 20)
       1 (get (integer_sequence 3 20) 1))
    (integer_sequence 3 20);
  [%verocaml.assert
    logical_update
      (integer_sequence 3 20)
      1 (get (integer_sequence 3 20) 1)
    = integer_sequence 3 20];
  ()
[@@verocaml.proof]

let explicit_slice_extensionality () : unit =
  axiom_extensionality
    (take (integer_sequence 4 30) 4)
    (integer_sequence 4 30);
  [%verocaml.assert
    take (integer_sequence 4 30) 4 = integer_sequence 4 30];
  ()
[@@verocaml.proof]

let explicit_append_extensionality () : unit =
  axiom_extensionality
    (logical_append (integer_sequence 3 40) (empty : int t))
    (integer_sequence 3 40);
  [%verocaml.assert
    logical_append (integer_sequence 3 40) (empty : int t)
    = integer_sequence 3 40];
  ()
[@@verocaml.proof]

let composed_pipeline (flag : bool) : unit =
  [%verocaml.assert length (composed_changed_sequence flag) = 4];
  [%verocaml.assert length (composed_framed_sequence flag) = 6];
  [%verocaml.assert length (composed_middle_sequence flag) = 4];
  [%verocaml.assert first (composed_middle_sequence flag) = 1];
  [%verocaml.assert last (composed_middle_sequence flag) = 4];
  [%verocaml.assert
    get (composed_middle_sequence flag) 1 = if flag then 20 else 2];
  [%verocaml.assert
    get (composed_middle_sequence flag) 2 = if flag then 3 else 30];
  ()
[@@verocaml.proof]
