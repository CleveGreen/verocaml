type binary =
  | Bv_add_mod
  | Bv_sub_mod
  | Bv_and
  | Bv_or
  | Bv_xor

type comparison =
  | Bv_unsigned_less_than
  | Bv_unsigned_less_or_equal
  | Bv_unsigned_greater_than
  | Bv_unsigned_greater_or_equal
  | Bv_signed_less_than
  | Bv_signed_less_or_equal
  | Bv_signed_greater_than
  | Bv_signed_greater_or_equal

val binary_name : binary -> string
val comparison_name : comparison -> string
