let requires (_ : unit -> bool) = ()
let ensures (_ : 'a -> bool) = ()
let decreases (_ : unit -> 'a) = ()
let assert_ (_ : unit -> bool) = ()
let old value = value
let marker (_ : string) = ()
let sidecar (_ : string) = ()

let spec_definition (_ : string) (_ : unit -> 'a) : 'a =
  failwith "Vero_ghost.spec_definition is a retained verification carrier"

let recursive_spec_definition (_ : string) (_ : string) (_ : unit -> 'a) : 'a =
  failwith
    "Vero_ghost.recursive_spec_definition is a retained verification carrier"

let proof_definition (_ : string) (_ : unit -> 'a) : 'a =
  failwith "Vero_ghost.proof_definition is a retained verification carrier"

let proof_region (_ : string) (_ : unit -> unit) : unit =
  failwith "Vero_ghost.proof_region is a retained verification carrier"

let type_invariant_definition (_ : string) (_ : unit -> 'a) : 'a =
  failwith
    "Vero_ghost.type_invariant_definition is a retained verification carrier"

let use_type_invariant (_ : string) (_ : 'a @ read) : unit =
  failwith "Vero_ghost.use_type_invariant is a retained verification carrier"

let reveal (_ : string) (_ : 'a) : unit =
  failwith "Vero_ghost.reveal is a retained verification carrier"

let reveal_with_fuel (_ : string) (_ : string) (_ : 'a) : unit =
  failwith "Vero_ghost.reveal_with_fuel is a retained verification carrier"

let external_specification (_ : string) (_ : unit -> 'a) : 'a =
  failwith "Vero_ghost.external_specification is a retained verification carrier"

let external_body (_ : string) (_ : unit -> 'a) : 'a =
  failwith "Vero_ghost.external_body is a retained verification carrier"
