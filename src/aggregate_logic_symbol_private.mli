val type_label : Vir.aggregate_type -> string
val suffix : Vir.aggregate_type -> string
val tag : Vir.aggregate_type -> string
val constructor : Vir.aggregate_type -> Sst.constructor_id -> string
val record_constructor : Vir.aggregate_type -> Sst.type_id -> string
val selector : range:string -> Vir.selector -> string
val named_sort : Vir.aggregate_type -> string
