open! Base
type 'a t = nativeint#

external unsafe_of_value
  :  'a @ local once
  -> 'a t @ once

  = "caml_native_pointer_of_value_bytecode" "caml_native_pointer_of_value"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external unsafe_to_value
  :  'a t
  -> 'a

  = "caml_native_pointer_to_value_bytecode" "caml_native_pointer_to_value"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external box_nativeint : nativeint# -> (nativeint[@local_opt]) = "%box_nativeint"
external unbox_nativeint : (nativeint[@local_opt]) -> nativeint# = "%unbox_nativeint"
external unbox_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"

let[@inline always] [@zero_alloc] null () = unbox_nativeint Nativeint.zero
let[@inline always] [@zero_alloc] equal x y = Nativeint.equal (box_nativeint x) (box_nativeint y)

module Imm = struct
  type 'a ptr = 'a t
  type 'a t = int

  external of_i64 : int64# -> 'a t = "%reinterpret_unboxed_int64_as_tagged_int63"

  let[@inline always] [@zero_alloc] of_ptr ptr =
    of_i64 (unbox_int64 (Int64.of_nativeint (box_nativeint ptr)))

  external to_ptr
    :  'a t
    -> 'a ptr

    = "caml_ext_pointer_as_native_pointer_bytecode" "caml_ext_pointer_as_native_pointer"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

let[@inline never] exercise () =
  let z = null () in
  assert (equal z z);
  let p = unsafe_of_value (ref 42) in
  assert (not (equal p z));
  let encoded = Imm.of_ptr p in
  assert (equal p (Imm.to_ptr encoded));
  let recovered : int ref = unsafe_to_value p in
  assert (!recovered = 42)

let () = exercise ()
