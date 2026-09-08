val encode : schema:string -> string list -> string
val decode : schema:string -> field_count:int -> string -> (string list, string) result
val digest : domain:string -> string -> string
val nonempty : label:string -> string -> (unit, string) result
val positive : label:string -> int -> (unit, string) result
val unique : label:string -> string list -> (unit, string) result
val sorted_unique : label:string -> string list -> (string list, string) result
val canonical_members : full_key:('a -> string) -> 'a list -> 'a list
val bool : bool -> string
val int : int -> string
val list : string list -> string
val decode_list : string -> (string list, string) result
