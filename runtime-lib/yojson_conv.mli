(** Utility Module for Yojson Conversions *)

(** {6 Conversion of OCaml-values to Yojsons} *)

(** [yojson_of_unit ()] converts a value of type [unit] to an Yojson. *)
val yojson_of_unit : unit -> Yojson.Safe.t

(** [yojson_of_bool b] converts the value [b] of type [bool] to an
    Yojson. *)
val yojson_of_bool : bool -> Yojson.Safe.t

(** [yojson_of_string str] converts the value [str] of type [string] to an
    Yojson. *)
val yojson_of_string : string -> Yojson.Safe.t

(** [yojson_of_bytes str] converts the value [str] of type [bytes] to an
    Yojson. *)
val yojson_of_bytes : bytes -> Yojson.Safe.t

(** [yojson_of_char c] converts the value [c] of type [char] to an
    Yojson. *)
val yojson_of_char : char -> Yojson.Safe.t

(** [yojson_of_int n] converts the value [n] of type [int] to an
    Yojson. *)
val yojson_of_int : int -> Yojson.Safe.t

(** [yojson_of_float n] converts the value [n] of type [float] to an
    Yojson. *)
val yojson_of_float : float -> Yojson.Safe.t

(** [yojson_of_int32 n] converts the value [n] of type [int32] to an
    Yojson. *)
val yojson_of_int32 : int32 -> Yojson.Safe.t

(** [yojson_of_int64 n] converts the value [n] of type [int64] to an
    Yojson. *)
val yojson_of_int64 : int64 -> Yojson.Safe.t

(** [yojson_of_nativeint n] converts the value [n] of type [nativeint] to an
    Yojson. *)
val yojson_of_nativeint : nativeint -> Yojson.Safe.t

(** [yojson_of_ref conv r] converts the value [r] of type ['a ref] to
    an Yojson.  Uses [conv] to convert values of type ['a] to an
    Yojson. *)
val yojson_of_ref : ('a -> Yojson.Safe.t) -> 'a ref -> Yojson.Safe.t

(** [yojson_of_lazy_t conv l] converts the value [l] of type ['a lazy_t] to
    an Yojson.  Uses [conv] to convert values of type ['a] to an
    Yojson. *)
val yojson_of_lazy_t : ('a -> Yojson.Safe.t) -> 'a lazy_t -> Yojson.Safe.t

(** [yojson_of_option conv opt] converts the value [opt] of type ['a
    option] to an Yojson.  Uses [conv] to convert values of type
    ['a] to an Yojson. *)
val yojson_of_option : ('a -> Yojson.Safe.t) -> 'a option -> Yojson.Safe.t

(** [yojson_of_pair conv1 conv2 pair] converts a pair to an Yojson.
    It uses its first argument to convert the first element of the pair,
    and its second argument to convert the second element of the pair. *)
val yojson_of_pair
  :  ('a -> Yojson.Safe.t)
  -> ('b -> Yojson.Safe.t)
  -> 'a * 'b
  -> Yojson.Safe.t

(** [yojson_of_triple conv1 conv2 conv3 triple] converts a triple to
    an Yojson using [conv1], [conv2], and [conv3] to convert its
    elements. *)
val yojson_of_triple
  :  ('a -> Yojson.Safe.t)
  -> ('b -> Yojson.Safe.t)
  -> ('c -> Yojson.Safe.t)
  -> 'a * 'b * 'c
  -> Yojson.Safe.t

(** [yojson_of_list conv lst] converts the value [lst] of type ['a
    list] to an Yojson.  Uses [conv] to convert values of type
    ['a] to an Yojson. *)
val yojson_of_list : ('a -> Yojson.Safe.t) -> 'a list -> Yojson.Safe.t

(** [yojson_of_array conv ar] converts the value [ar] of type ['a
    array] to an Yojson.  Uses [conv] to convert values of type
    ['a] to an Yojson. *)
val yojson_of_array : ('a -> Yojson.Safe.t) -> 'a array -> Yojson.Safe.t

(** [yojson_of_hashtbl conv_key conv_value htbl] converts the value [htbl]
    of type [('a, 'b) Hashtbl.t] to an Yojson.  Uses [conv_key]
    to convert the hashtable keys of type ['a], and [conv_value] to
    convert hashtable values of type ['b] to Yojsons. *)
val yojson_of_hashtbl
  :  ('a -> Yojson.Safe.t)
  -> ('b -> Yojson.Safe.t)
  -> ('a, 'b) Hashtbl.t
  -> Yojson.Safe.t

(** [yojson_of_opaque x] converts the value [x] of opaque type to an
    Yojson.  This means the user need not provide converters,
    but the result cannot be interpreted. *)
val yojson_of_opaque : 'a -> Yojson.Safe.t

(** [yojson_of_fun f] converts the value [f] of function type to a
    dummy Yojson.  Functions cannot be serialized as Yojsons,
    but at least a placeholder can be generated for pretty-printing. *)
val yojson_of_fun : ('a -> 'b) -> Yojson.Safe.t

(** {6 Conversion of Yojsons to OCaml-values} *)

(** [Of_yojson_error (exn, yojson)] the exception raised when an Yojson
    could not be successfully converted to an OCaml-value. *)
exception Of_yojson_error of exn * Yojson.Safe.t

(** [record_check_extra_fields] checks for extra (= unknown) fields
    in record Yojsons. *)
val record_check_extra_fields : bool ref

(** [of_yojson_error reason yojson] @raise Of_yojson_error (Failure reason, yojson). *)
val of_yojson_error : string -> Yojson.Safe.t -> 'a

(** [of_yojson_error_exn exc yojson] @raise Of_yojson_error (exc, yojson). *)
val of_yojson_error_exn : exn -> Yojson.Safe.t -> 'a

(** [unit_of_yojson yojson] converts Yojson [yojson] to a value of type
    [unit]. *)
val unit_of_yojson : Yojson.Safe.t -> unit

(** [bool_of_yojson yojson] converts Yojson [yojson] to a value of type
    [bool]. *)
val bool_of_yojson : Yojson.Safe.t -> bool

(** [string_of_yojson yojson] converts Yojson [yojson] to a value of type
    [string]. *)
val string_of_yojson : Yojson.Safe.t -> string

(** [bytes_of_yojson yojson] converts Yojson [yojson] to a value of type
    [bytes]. *)
val bytes_of_yojson : Yojson.Safe.t -> bytes

(** [char_of_yojson yojson] converts Yojson [yojson] to a value of type
    [char]. *)
val char_of_yojson : Yojson.Safe.t -> char

(** [int_of_yojson yojson] converts Yojson [yojson] to a value of type
    [int]. *)
val int_of_yojson : Yojson.Safe.t -> int

(** [float_of_yojson yojson] converts Yojson [yojson] to a value of type
    [float]. *)
val float_of_yojson : Yojson.Safe.t -> float

(** [int32_of_yojson yojson] converts Yojson [yojson] to a value of type
    [int32]. *)
val int32_of_yojson : Yojson.Safe.t -> int32

(** [int64_of_yojson yojson] converts Yojson [yojson] to a value of type
    [int64]. *)
val int64_of_yojson : Yojson.Safe.t -> int64

(** [nativeint_of_yojson yojson] converts Yojson [yojson] to a value
    of type [nativeint]. *)
val nativeint_of_yojson : Yojson.Safe.t -> nativeint

(** [ref_of_yojson conv yojson] converts Yojson [yojson] to a value
    of type ['a ref] using conversion function [conv], which converts
    an Yojson to a value of type ['a]. *)
val ref_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a ref

(** [lazy_t_of_yojson conv yojson] converts Yojson [yojson] to a value
    of type ['a lazy_t] using conversion function [conv], which converts
    an Yojson to a value of type ['a]. *)
val lazy_t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a lazy_t

(** [option_of_yojson conv yojson] converts Yojson [yojson] to a value
    of type ['a option] using conversion function [conv], which converts
    an Yojson to a value of type ['a]. *)
val option_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a option

(** [pair_of_yojson conv1 conv2 yojson] converts Yojson [yojson] to a pair
    of type ['a * 'b] using conversion functions [conv1] and [conv2],
    which convert Yojsons to values of type ['a] and ['b]
    respectively. *)
val pair_of_yojson
  :  (Yojson.Safe.t -> 'a)
  -> (Yojson.Safe.t -> 'b)
  -> Yojson.Safe.t
  -> 'a * 'b

(** [triple_of_yojson conv1 conv2 conv3 yojson] converts Yojson [yojson]
    to a triple of type ['a * 'b * 'c] using conversion functions [conv1],
    [conv2], and [conv3], which convert Yojsons to values of type
    ['a], ['b], and ['c] respectively. *)
val triple_of_yojson
  :  (Yojson.Safe.t -> 'a)
  -> (Yojson.Safe.t -> 'b)
  -> (Yojson.Safe.t -> 'c)
  -> Yojson.Safe.t
  -> 'a * 'b * 'c

(** [list_of_yojson conv yojson] converts Yojson [yojson] to a value
    of type ['a list] using conversion function [conv], which converts
    an Yojson to a value of type ['a]. *)
val list_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a list

(** [array_of_yojson conv yojson] converts Yojson [yojson] to a value
    of type ['a array] using conversion function [conv], which converts
    an Yojson to a value of type ['a]. *)
val array_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a array

(** [hashtbl_of_yojson conv_key conv_value yojson] converts Yojson
    [yojson] to a value of type [('a, 'b) Hashtbl.t] using conversion
    function [conv_key], which converts an Yojson to hashtable
    key of type ['a], and function [conv_value], which converts an
    Yojson to hashtable value of type ['b]. *)
val hashtbl_of_yojson
  :  (Yojson.Safe.t -> 'a)
  -> (Yojson.Safe.t -> 'b)
  -> Yojson.Safe.t
  -> ('a, 'b) Hashtbl.t

(** [opaque_of_yojson yojson] @raise Of_yojson_error when attempting to
    convert an Yojson to an opaque value. *)
val opaque_of_yojson : Yojson.Safe.t -> 'a

(** [fun_of_yojson yojson] @raise Of_yojson_error when attempting to
    convert an Yojson to a function. *)
val fun_of_yojson : Yojson.Safe.t -> 'a

module Result : sig
  type 'a t = ('a, string) result

  val unpack : (Yojson.Safe.t -> 'a t) -> Yojson.Safe.t -> 'a
  val pack : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
  val of_exn : exn -> 'a t
end

module Primitives : sig
  val yojson_of_unit : unit -> Yojson.Safe.t
  val yojson_of_bool : bool -> Yojson.Safe.t
  val yojson_of_string : string -> Yojson.Safe.t
  val yojson_of_bytes : bytes -> Yojson.Safe.t
  val yojson_of_char : char -> Yojson.Safe.t
  val yojson_of_int : int -> Yojson.Safe.t
  val yojson_of_float : float -> Yojson.Safe.t
  val yojson_of_int32 : int32 -> Yojson.Safe.t
  val yojson_of_int64 : int64 -> Yojson.Safe.t
  val yojson_of_nativeint : nativeint -> Yojson.Safe.t
  val yojson_of_ref : ('a -> Yojson.Safe.t) -> 'a ref -> Yojson.Safe.t
  val yojson_of_lazy_t : ('a -> Yojson.Safe.t) -> 'a lazy_t -> Yojson.Safe.t
  val yojson_of_option : ('a -> Yojson.Safe.t) -> 'a option -> Yojson.Safe.t
  val yojson_of_list : ('a -> Yojson.Safe.t) -> 'a list -> Yojson.Safe.t
  val yojson_of_array : ('a -> Yojson.Safe.t) -> 'a array -> Yojson.Safe.t

  val yojson_of_hashtbl
    :  ('a -> Yojson.Safe.t)
    -> ('b -> Yojson.Safe.t)
    -> ('a, 'b) Hashtbl.t
    -> Yojson.Safe.t

  val unit_of_yojson : Yojson.Safe.t -> unit
  val bool_of_yojson : Yojson.Safe.t -> bool
  val string_of_yojson : Yojson.Safe.t -> string
  val bytes_of_yojson : Yojson.Safe.t -> bytes
  val char_of_yojson : Yojson.Safe.t -> char
  val int_of_yojson : Yojson.Safe.t -> int
  val float_of_yojson : Yojson.Safe.t -> float
  val int32_of_yojson : Yojson.Safe.t -> int32
  val int64_of_yojson : Yojson.Safe.t -> int64
  val nativeint_of_yojson : Yojson.Safe.t -> nativeint
  val ref_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a ref
  val lazy_t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a lazy_t
  val option_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a option
  val list_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a list
  val array_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a array

  val hashtbl_of_yojson
    :  (Yojson.Safe.t -> 'a)
    -> (Yojson.Safe.t -> 'b)
    -> Yojson.Safe.t
    -> ('a, 'b) Hashtbl.t
end
