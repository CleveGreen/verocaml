#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <limits.h>
#include <stdio.h>

CAMLprim value verocaml_bv_c_unsigned_facts(value unit)
{
  CAMLparam1(unit);
  CAMLlocal3(pair, bits, maximum);
  char maximum_text[3 * sizeof(unsigned) + 1];

  snprintf(maximum_text, sizeof(maximum_text), "%u", UINT_MAX);
  bits = Val_int((int)(sizeof(unsigned) * CHAR_BIT));
  maximum = caml_copy_string(maximum_text);
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, bits);
  Store_field(pair, 1, maximum);
  CAMLreturn(pair);
}
