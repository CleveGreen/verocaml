type resolved
type error = Unsupported of Sst.verification_policy

val default : resolved
val resolve : Sst.verification_policy -> (resolved, error) result
val policy : resolved -> Sst.verification_policy
val to_string : resolved -> string
val error_to_string : error -> string
