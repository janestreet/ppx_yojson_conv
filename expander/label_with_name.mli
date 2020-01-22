open! Base

type t

val create : label:string -> name_override:string option -> t
val of_constructor_declaration : Ppxlib.constructor_declaration -> t
val label : t -> string
val name : t -> string
