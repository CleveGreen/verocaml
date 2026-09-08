open Parsetree

val attribute_name : string

val explicit_type : value_binding -> core_type option

val declaration_attribute :
  value_binding -> name:string Location.loc -> core_type -> attribute

