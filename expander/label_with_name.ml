open! Base

type t =
  { label : string
  ; name_override : string option
  }

let create ~label ~name_override = { label; name_override }

let of_constructor_declaration (cd : Ppxlib.constructor_declaration) =
  let label = cd.pcd_name.txt in
  let name_override = Ppxlib.Attribute.get Attrs.yojson_variant_name cd in
  create ~label ~name_override
;;

let label t = t.label
let name t = Option.value t.name_override ~default:t.label
