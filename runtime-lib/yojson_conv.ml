open StdLabels
open MoreLabels
open! Yojson

type t = Yojson.Safe.t

let yojson_of_unit () : t = `Null
let yojson_of_bool b : t = `Bool b
let yojson_of_string str : t = `String str
let yojson_of_bytes bytes : t = `String (Bytes.to_string bytes)
let yojson_of_char c : t = `String (String.make 1 c)
let yojson_of_int n : t = `Int n
let yojson_of_float n : t = `Float n
let yojson_of_int32 (n : Int32.t) : t = `Intlit (Int32.to_string n)
let yojson_of_int64 (n : Int64.t) : t = `Intlit (Int64.to_string n)
let yojson_of_nativeint n : t = `Intlit (Nativeint.to_string n)
let yojson_of_ref yojson_of__a rf = yojson_of__a !rf
let yojson_of_lazy_t yojson_of__a lv = yojson_of__a (Lazy.force lv)

let yojson_of_option yojson_of__a = function
  | Some x -> yojson_of__a x
  | None -> `Null
;;

let yojson_of_pair yojson_of__a yojson_of__b (a, b) =
  `List [ yojson_of__a a; yojson_of__b b ]
;;

let yojson_of_triple yojson_of__a yojson_of__b yojson_of__c (a, b, c) =
  `List [ yojson_of__a a; yojson_of__b b; yojson_of__c c ]
;;

(* List.rev (List.rev_map ...) is tail recursive, the OCaml standard
   library List.map is NOT. *)
let yojson_of_list yojson_of__a lst = `List (List.rev (List.rev_map ~f:yojson_of__a lst))

let yojson_of_array yojson_of__a ar =
  let lst_ref = ref [] in
  for i = Array.length ar - 1 downto 0 do
    lst_ref := yojson_of__a ar.(i) :: !lst_ref
  done;
  `List !lst_ref
;;

let yojson_of_hashtbl yojson_of_key yojson_of_val htbl =
  let coll ~key:k ~data:v acc = `List [ yojson_of_key k; yojson_of_val v ] :: acc in
  `List (Hashtbl.fold htbl ~init:[] ~f:coll)
;;

let yojson_of_opaque _ = `String "<opaque>"
let yojson_of_fun _ = `String "<fun>"

exception Of_yojson_error of exn * t

let record_check_extra_fields = ref true
let of_yojson_error_exn exc yojson = raise (Of_yojson_error (exc, yojson))
let of_yojson_error what yojson = raise (Of_yojson_error (Failure what, yojson))

let unit_of_yojson yojson =
  match yojson with
  | `Null -> ()
  | _ -> of_yojson_error "unit_of_yojson: `Null needed" yojson
;;

let bool_of_yojson yojson =
  match yojson with
  | `Bool v -> v
  | _ -> of_yojson_error "bool_of_yojson: true/false needed" yojson
;;

let string_of_yojson yojson =
  match yojson with
  | `String str -> str
  | _ -> of_yojson_error "string_of_yojson: string needed" yojson
;;

let bytes_of_yojson yojson =
  match yojson with
  | `String str -> Bytes.of_string str
  | _ -> of_yojson_error "bytes_of_yojson: string needed" yojson
;;

let char_of_yojson yojson =
  match yojson with
  | `String str ->
    if String.length str <> 1
    then of_yojson_error "char_of_yojson: string must contain one character only" yojson;
    str.[0]
  | _ -> of_yojson_error "char_of_yojson: string of size one needed" yojson
;;

let int_of_yojson yojson =
  match yojson with
  | `Int v -> v
  | _ -> of_yojson_error "int_of_yojson: integer needed" yojson
;;

let float_of_yojson yojson =
  match yojson with
  | `Float v -> v
  | `Int i -> float_of_int i
  | `Intlit str -> float_of_string str
  | _ -> of_yojson_error "float_of_yojson: float needed" yojson
;;

let int32_of_yojson yojson =
  match yojson with
  | `Intlit str -> Int32.of_string str
  | `Int v -> Int32.of_int v
  | _ -> of_yojson_error "int32_of_yojson: integer needed" yojson
;;

let int64_of_yojson yojson =
  match yojson with
  | `Intlit str -> Int64.of_string str
  | `Int v -> Int64.of_int v
  | _ -> of_yojson_error "int64_of_yojson: integer needed" yojson
;;

let nativeint_of_yojson yojson =
  match yojson with
  | `Intlit str -> Nativeint.of_string str
  | `Int v -> Nativeint.of_int v
  | _ -> of_yojson_error "nativeint_of_yojson: integer needed" yojson
;;

let ref_of_yojson a__of_yojson yojson = ref (a__of_yojson yojson)
let lazy_t_of_yojson a__of_yojson yojson = Lazy.from_val (a__of_yojson yojson)

let option_of_yojson a__of_yojson yojson =
  match yojson with
  | `Null -> None
  | el -> Some (a__of_yojson el)
;;

let pair_of_yojson a__of_yojson b__of_yojson yojson =
  match yojson with
  | `List [ a_yojson; b_yojson ] ->
    let a = a__of_yojson a_yojson in
    let b = b__of_yojson b_yojson in
    a, b
  | _ -> of_yojson_error "pair_of_yojson: invalid format" yojson
;;

let triple_of_yojson a__of_yojson b__of_yojson c__of_yojson yojson =
  match yojson with
  | `List [ a_yojson; b_yojson; c_yojson ] ->
    let a = a__of_yojson a_yojson in
    let b = b__of_yojson b_yojson in
    let c = c__of_yojson c_yojson in
    a, b, c
  | _ -> of_yojson_error "triple_of_yojson: invalid format" yojson
;;

let list_of_yojson a__of_yojson yojson =
  match yojson with
  | `List lst ->
    let rev_lst = List.rev_map lst ~f:a__of_yojson in
    List.rev rev_lst
  | _ -> of_yojson_error "list_of_yojson: list needed" yojson
;;

let array_of_yojson a__of_yojson yojson =
  match yojson with
  | `List [] -> [||]
  | `List (h :: t) ->
    let len = List.length t + 1 in
    let res = Array.make len (a__of_yojson h) in
    let rec loop i = function
      | [] -> res
      | h :: t ->
        res.(i) <- a__of_yojson h;
        loop (i + 1) t
    in
    loop 1 t
  | _ -> of_yojson_error "array_of_yojson: list needed" yojson
;;

let hashtbl_of_yojson key_of_yojson val_of_yojson yojson =
  match yojson with
  | `List lst ->
    let htbl = Hashtbl.create 0 in
    let act = function
      | `List [ k_yojson; v_yojson ] ->
        Hashtbl.add htbl ~key:(key_of_yojson k_yojson) ~data:(val_of_yojson v_yojson)
      | _ -> of_yojson_error "hashtbl_of_yojson: tuple list needed" yojson
    in
    List.iter lst ~f:act;
    htbl
  | _ -> of_yojson_error "hashtbl_of_yojson: list needed" yojson
;;

let opaque_of_yojson yojson =
  of_yojson_error "opaque_of_yojson: cannot convert opaque values" yojson
;;

let fun_of_yojson yojson =
  of_yojson_error "fun_of_yojson: cannot convert function values" yojson
;;

module Result = struct
  type 'a t = ('a, string) result

  let unpack (f : Safe.t -> 'a t) x : 'a =
    match f x with
    | Ok v -> v
    | Error str -> of_yojson_error str x
  ;;

  let of_exn = function
    | Of_yojson_error (Failure s, _json) -> Error s
    | exn -> Error (Printexc.to_string exn)
  ;;

  let pack (f : Safe.t -> 'a) x : 'a t =
    match f x with
    | v -> Ok v
    | exception exn -> of_exn exn
  ;;
end

module Primitives = struct
  let yojson_of_array = yojson_of_array
  let array_of_yojson = array_of_yojson
  let yojson_of_bool = yojson_of_bool
  let bool_of_yojson = bool_of_yojson
  let yojson_of_char = yojson_of_char
  let char_of_yojson = char_of_yojson
  let yojson_of_float = yojson_of_float
  let float_of_yojson = float_of_yojson
  let yojson_of_int = yojson_of_int
  let int_of_yojson = int_of_yojson
  let yojson_of_int32 = yojson_of_int32
  let int32_of_yojson = int32_of_yojson
  let yojson_of_int64 = yojson_of_int64
  let int64_of_yojson = int64_of_yojson
  let yojson_of_list = yojson_of_list
  let list_of_yojson = list_of_yojson
  let yojson_of_nativeint = yojson_of_nativeint
  let nativeint_of_yojson = nativeint_of_yojson
  let yojson_of_option = yojson_of_option
  let option_of_yojson = option_of_yojson
  let yojson_of_ref = yojson_of_ref
  let ref_of_yojson = ref_of_yojson
  let yojson_of_string = yojson_of_string
  let string_of_yojson = string_of_yojson
  let yojson_of_bytes = yojson_of_bytes
  let bytes_of_yojson = bytes_of_yojson
  let yojson_of_unit = yojson_of_unit
  let unit_of_yojson = unit_of_yojson
  let yojson_of_lazy_t = yojson_of_lazy_t
  let lazy_t_of_yojson = lazy_t_of_yojson
  let yojson_of_hashtbl = yojson_of_hashtbl
  let hashtbl_of_yojson = hashtbl_of_yojson
end
