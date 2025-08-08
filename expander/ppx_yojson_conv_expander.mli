open Ppxlib

module Attrs : sig
  val default : (label_declaration, expression) Attribute.t
  val drop_default : (label_declaration, expression option) Attribute.t
  val drop_if : (label_declaration, expression) Attribute.t
end

module Yojson_of : sig
  val type_extension : core_type -> core_type
  val core_type : core_type -> expression

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> Capitalization_ppx_configuration.t option
    -> structure
end

module Yojson_fields : sig
  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> Capitalization_ppx_configuration.t option
    -> structure
end

module Of_yojson : sig
  val type_extension : core_type -> core_type
  val core_type : path:string -> core_type -> expression

  val sig_type_decl
    :  poly:bool
    -> loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list

  val str_type_decl
    :  loc:Location.t
    -> poly:bool
    -> path:string
    -> rec_flag * type_declaration list
    -> Capitalization_ppx_configuration.t option
    -> structure
end

module Sig_yojson : sig
  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list
end
