type t

val build : Sst_validation.validated_program -> t
val find : t -> Sst.function_id -> Sst.function_definition option
val declarations : t -> Sst.function_definition list
