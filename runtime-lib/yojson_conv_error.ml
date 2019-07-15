(* Conv_error: Module for Handling Errors during Automated Yojson
   Conversions *)

open! StdLabels
open Yojson_conv

let sprintf = Printf.sprintf

(* Errors concerning tuples *)

let tuple_of_size_n_expected loc n yojson =
  of_yojson_error (sprintf "%s_of_yojson: tuple of size %d expected" loc n) yojson
;;

(* Errors concerning sum types *)

let stag_no_args loc yojson =
  of_yojson_error (loc ^ "_of_yojson: sum tag does not take arguments") yojson
;;

let stag_incorrect_n_args loc tag yojson =
  let msg =
    sprintf "%s_of_yojson: sum tag %S has incorrect number of arguments" loc tag
  in
  of_yojson_error msg yojson
;;

let stag_takes_args loc yojson =
  of_yojson_error (loc ^ "_of_yojson: sum tag must be a structured value") yojson
;;

let nested_list_invalid_sum loc yojson =
  of_yojson_error (loc ^ "_of_yojson: a nested list is an invalid sum") yojson
;;

let empty_list_invalid_sum loc yojson =
  of_yojson_error (loc ^ "_of_yojson: the empty list is an invalid sum") yojson
;;

let unexpected_stag loc yojson =
  of_yojson_error (loc ^ "_of_yojson: unexpected sum tag") yojson
;;

(* Errors concerning records *)

let record_superfluous_fields ~what ~loc rev_fld_names yojson =
  let fld_names_str = String.concat (List.rev rev_fld_names) ~sep:" " in
  let msg = sprintf "%s_of_yojson: %s: %s" loc what fld_names_str in
  of_yojson_error msg yojson
;;

let record_duplicate_fields loc rev_fld_names yojson =
  record_superfluous_fields ~what:"duplicate fields" ~loc rev_fld_names yojson
;;

let record_extra_fields loc rev_fld_names yojson =
  record_superfluous_fields ~what:"extra fields" ~loc rev_fld_names yojson
;;

let rec record_get_undefined_loop fields = function
  | [] -> String.concat (List.rev fields) ~sep:" "
  | (true, field) :: rest -> record_get_undefined_loop (field :: fields) rest
  | _ :: rest -> record_get_undefined_loop fields rest
;;

let record_undefined_elements loc yojson lst =
  let undefined = record_get_undefined_loop [] lst in
  let msg =
    sprintf
      "%s_of_yojson: the following record elements were undefined: %s"
      loc
      undefined
  in
  of_yojson_error msg yojson
;;

let record_list_instead_atom loc yojson =
  let msg = loc ^ "_of_yojson: list instead of atom for record expected" in
  of_yojson_error msg yojson
;;

let record_poly_field_value loc yojson =
  let msg =
    loc
    ^ "_of_yojson: cannot convert values of types resulting from polymorphic record \
       fields"
  in
  of_yojson_error msg yojson
;;

(* Errors concerning polymorphic variants *)

exception No_variant_match

let no_variant_match () = raise No_variant_match

let no_matching_variant_found loc yojson =
  of_yojson_error (loc ^ "_of_yojson: no matching variant found") yojson
;;

let ptag_no_args loc yojson =
  of_yojson_error
    (loc ^ "_of_yojson: polymorphic variant does not take arguments")
    yojson
;;

let ptag_incorrect_n_args loc cnstr yojson =
  let msg =
    sprintf
      "%s_of_yojson: polymorphic variant tag %S has incorrect number of arguments"
      loc
      cnstr
  in
  of_yojson_error msg yojson
;;

let ptag_takes_args loc yojson =
  of_yojson_error (loc ^ "_of_yojson: polymorphic variant tag takes an argument") yojson
;;

let nested_list_invalid_poly_var loc yojson =
  of_yojson_error
    (loc ^ "_of_yojson: a nested list is an invalid polymorphic variant")
    yojson
;;

let empty_list_invalid_poly_var loc yojson =
  of_yojson_error
    (loc ^ "_of_yojson: the empty list is an invalid polymorphic variant")
    yojson
;;

let empty_type loc yojson =
  of_yojson_error (loc ^ "_of_yojson: trying to convert an empty type") yojson
;;
