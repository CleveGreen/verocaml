val lower :
  ?allow_imported_opens:bool ->
  Cmt_input.implementation -> (Sst.program, Diagnostic.t) result

val lower_file :
  ?int_size:int ->
  ?allow_imported_opens:bool ->
  string ->
  (Sst.program, Diagnostic.t) result
