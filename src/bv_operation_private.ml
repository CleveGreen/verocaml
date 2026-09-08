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

let binary_name = function
  | Bv_add_mod -> "add-mod"
  | Bv_sub_mod -> "sub-mod"
  | Bv_and -> "and"
  | Bv_or -> "or"
  | Bv_xor -> "xor"

let comparison_name = function
  | Bv_unsigned_less_than -> "unsigned-less-than"
  | Bv_unsigned_less_or_equal -> "unsigned-less-or-equal"
  | Bv_unsigned_greater_than -> "unsigned-greater-than"
  | Bv_unsigned_greater_or_equal -> "unsigned-greater-or-equal"
  | Bv_signed_less_than -> "signed-less-than"
  | Bv_signed_less_or_equal -> "signed-less-or-equal"
  | Bv_signed_greater_than -> "signed-greater-than"
  | Bv_signed_greater_or_equal -> "signed-greater-or-equal"
