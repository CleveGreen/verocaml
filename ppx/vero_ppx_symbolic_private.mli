open Parsetree

val rewrite_structure :
  keep_ghost:bool -> issuer:string -> structure -> structure
val rewrite_signature :
  keep_ghost:bool -> issuer:string -> signature -> signature
val issued_interface_attribute : attribute -> bool
