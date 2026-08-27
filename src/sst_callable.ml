type t = Sst_validation.validated_program

let build validated = validated

let find validated id =
  Option.map Sst_validation.callable_definition
    (Sst_validation.find_callable validated id)

let declarations validated =
  List.map Sst_validation.callable_definition
    (Sst_validation.callable_descriptors validated)
