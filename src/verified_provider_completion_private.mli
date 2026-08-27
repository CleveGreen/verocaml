type t

val authenticates :
  t -> implementation:Cmt_input.implementation -> program:Sst.program -> bool

val authenticates_recursive_definition : t -> Sst.function_definition -> bool

module For_driver : sig
  val issue : implementation:Cmt_input.implementation -> program:Sst.program -> t
end

module For_testing : sig
  val forged : definition:Sst.function_definition -> t
end
