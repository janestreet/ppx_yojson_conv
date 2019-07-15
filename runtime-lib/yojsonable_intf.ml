module type S = sig
  type t

  val t_of_yojson : Yojson.Safe.t -> t
  val t_of_yojson' : Yojson.Safe.t -> t Yojson_conv.Result.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type S1 = sig
  type 'a t

  val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t

  val t_of_yojson'
    :  (Yojson.Safe.t -> 'a Yojson_conv.Result.t)
    -> Yojson.Safe.t
    -> 'a t Yojson_conv.Result.t

  val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
end

module type S2 = sig
  type ('a, 'b) t

  val t_of_yojson
    :  (Yojson.Safe.t -> 'a)
    -> (Yojson.Safe.t -> 'b)
    -> Yojson.Safe.t
    -> ('a, 'b) t

  val t_of_yojson'
    :  (Yojson.Safe.t -> 'a Yojson_conv.Result.t)
    -> (Yojson.Safe.t -> 'b Yojson_conv.Result.t)
    -> Yojson.Safe.t
    -> ('a, 'b) t Yojson_conv.Result.t

  val yojson_of_t
    :  ('a -> Yojson.Safe.t)
    -> ('b -> Yojson.Safe.t)
    -> ('a, 'b) t
    -> Yojson.Safe.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val t_of_yojson
    :  (Yojson.Safe.t -> 'a)
    -> (Yojson.Safe.t -> 'b)
    -> (Yojson.Safe.t -> 'c)
    -> Yojson.Safe.t
    -> ('a, 'b, 'c) t

  val t_of_yojson'
    :  (Yojson.Safe.t -> 'a Yojson_conv.Result.t)
    -> (Yojson.Safe.t -> 'b Yojson_conv.Result.t)
    -> (Yojson.Safe.t -> 'c Yojson_conv.Result.t)
    -> Yojson.Safe.t
    -> ('a, 'b, 'c) t Yojson_conv.Result.t

  val yojson_of_t
    :  ('a -> Yojson.Safe.t)
    -> ('b -> Yojson.Safe.t)
    -> ('c -> Yojson.Safe.t)
    -> ('a, 'b, 'c) t
    -> Yojson.Safe.t
end
