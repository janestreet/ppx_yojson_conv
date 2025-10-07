(* yojson_conv: Preprocessing Module for Automated Yojson Conversions *)

open Ppxlib
module Attrs = Ppx_yojson_conv_expander.Attrs

let capitalization_arg =
  Capitalization_ppx_configuration.argument ~ppx_name:"ppx_yojson_conv"
;;

module Yojson_of = struct
  module E = Ppx_yojson_conv_expander.Yojson_of

  let name = "yojson_of"

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      E.str_type_decl
      ~attributes:
        [ Attribute.T Attrs.default
        ; Attribute.T Attrs.drop_default
        ; Attribute.T Attrs.drop_if
        ]
  ;;

  let sig_type_decl = Deriving.Generator.make_noarg E.sig_type_decl
  let extension ~loc:_ ~path:_ ctyp = E.core_type ctyp
  let deriver = Deriving.add name ~str_type_decl ~sig_type_decl ~extension

  let () =
    Driver.register_transformation
      name
      ~rules:
        [ Context_free.Rule.extension
            (Extension.declare
               name
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
        ]
  ;;
end

module Yojson_fields = struct
  module E = Ppx_yojson_conv_expander.Yojson_fields

  let name = "yojson_fields"

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      E.str_type_decl
      ~attributes:[]
  ;;

  let deriver = Deriving.add name ~str_type_decl
end

module Of_yojson = struct
  module E = Ppx_yojson_conv_expander.Of_yojson

  let name = "of_yojson"

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      (E.str_type_decl ~poly:false)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl = Deriving.Generator.make_noarg (E.sig_type_decl ~poly:false)
  let extension ~loc:_ ~path ctyp = E.core_type ~path ctyp
  let deriver = Deriving.add name ~str_type_decl ~sig_type_decl ~extension

  let () =
    Driver.register_transformation
      name
      ~rules:
        [ Context_free.Rule.extension
            (Extension.declare
               name
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
        ]
  ;;
end

module Of_yojson_poly = struct
  module E = Ppx_yojson_conv_expander.Of_yojson

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      (E.str_type_decl ~poly:true)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl = Deriving.Generator.make_noarg (E.sig_type_decl ~poly:true)
  let deriver = Deriving.add "of_yojson_poly" ~sig_type_decl ~str_type_decl
end

let yojson_of = Yojson_of.deriver
let yojson_fields_of = Yojson_fields.deriver
let of_yojson = Of_yojson.deriver
let of_yojson_poly = Of_yojson_poly.deriver

module Yojson_in_sig = struct
  module E = Ppx_yojson_conv_expander.Sig_yojson

  let sig_type_decl = Deriving.Generator.make_noarg E.sig_type_decl

  let deriver =
    Deriving.add
      "ppx_yojson_conv: let this be a string that wouldn't parse if put in the source"
      ~sig_type_decl
  ;;
end

let yojson =
  Deriving.add_alias
    "yojson"
    [ yojson_of; of_yojson ]
    ~sig_type_decl:[ Yojson_in_sig.deriver ]
;;

let yojson_poly = Deriving.add_alias "yojson_poly" [ yojson_of; of_yojson_poly ]
