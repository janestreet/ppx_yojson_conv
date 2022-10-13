open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = float [@@deriving yojson]

module M : sig
  type t = float list [@@deriving yojson]
end = struct
  type nonrec t = t list [@@deriving yojson]
end

type 'a u = 'a [@@deriving yojson]

module M2 : sig
  type 'a u = 'a list [@@deriving yojson]
end = struct
  type nonrec 'a u = 'a u list [@@deriving yojson]
end

type 'a v = 'a w
and 'a w = A of 'a v [@@deriving yojson]

type 'a v_ = 'a v [@@deriving yojson]
type 'a w_ = 'a w [@@deriving yojson]

module M3 : sig
  type 'a v = 'a w_ [@@deriving yojson]
  type 'a w = 'a v_ [@@deriving yojson]
end = struct
  type nonrec 'a v = 'a w
  and 'a w = 'a v [@@deriving yojson]
end

type t0 = A of t0 [@@deriving yojson]

module B : sig
  type nonrec t0 = t0 [@@deriving yojson]
end = struct
  type nonrec t0 = t0 = A of t0 [@@deriving yojson]
end

type t1 = A of t2
and t2 = B of t1 [@@deriving yojson]

module C : sig
  type nonrec t1 = t1 [@@deriving yojson]
  type nonrec t2 = t2 [@@deriving yojson]
end = struct
  type nonrec t1 = t1 = A of t2
  and t2 = t2 = B of t1 [@@deriving yojson]
end

type 'a v1 = A of 'a v2
and 'a v2 = B of 'a v1 [@@deriving yojson]

module D : sig
  type nonrec 'a v1 = 'a v1 [@@deriving yojson]
  type nonrec 'a v2 = 'a v2 [@@deriving yojson]
end = struct
  type nonrec 'a v1 = 'a v1 = A of 'a v2
  and 'a v2 = 'a v2 = B of 'a v1 [@@deriving yojson]
end

type +'a w1

module E = struct
  type nonrec +'a w1 = 'a w1
end

type 'a y1 = A of 'a y2
and 'a y2 = B of 'a y1

module F : sig
  type nonrec 'a y2 = B of 'a y1
  type nonrec 'a y1 = 'a y1
end = struct
  type nonrec 'a y1 = 'a y1 = A of 'a y2
  and 'a y2 = B of 'a y1
end

type z1 = A of z1

module G : sig
  module A : sig
    type z2 = A of z2
  end

  module B : sig
    type z2 = A of z2
  end

  module C : sig
    type z2 = A of z2
  end
end = struct
  type z2 = z1 = A of z1

  module A = struct
    type nonrec z2 = z1 = A of z2
  end

  module B = struct
    type nonrec z2 = z2 = A of z2
  end

  module C = struct
    type nonrec z2 = z2 = A of z1
  end
end

type ('a, 'b) zz = A of 'a * 'b

module H = struct
  type nonrec ('a, 'b) zz = ('a, 'b) zz = A of 'a * 'b
end

module I = struct
  type nonrec 'a zz = ('a, 'a) zz
end
