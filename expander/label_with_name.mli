open! Base

type t

val create
  :  label:string
  -> name_override:string option
  -> capitalization:Capitalization_ppx_configuration.t option
  -> t

val of_constructor_declaration
  :  Ppxlib.constructor_declaration
  -> capitalization:Capitalization_ppx_configuration.t option
  -> t

val label : t -> string
val name : t -> string
