open! Base

type t =
  { label : string
  ; name_override : string option
  }

let create ~label ~name_override ~capitalization =
  let name_override =
    match name_override, capitalization with
    | Some s, _ -> Some s
    | None, Some c ->
      Some (Capitalization_ppx_configuration.apply_to_snake_case_exn c label)
    | None, None -> None
  in
  { label; name_override }
;;

let of_constructor_declaration (cd : Ppxlib.constructor_declaration) =
  let label = cd.pcd_name.txt in
  let name_override = Ppxlib.Attribute.get Attrs.yojson_variant_name cd in
  create ~label ~name_override
;;

let label t = t.label
let name t = Option.value t.name_override ~default:t.label
