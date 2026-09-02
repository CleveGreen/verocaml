type entrypoint = Implementation | Interface

val make : ?entrypoint:entrypoint -> string list -> Ast_mapper.mapper
