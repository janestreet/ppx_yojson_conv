open Base
open Base_yojson
open Expect_test_helpers_core

(* Module names below are used in error messages being tested. *)
[@@@warning "-unused-module"]

include struct
  [@@@ocaml.warning "-32"]

  let ( ! ) _x = `Shadowed
  let ignore _x = `Shadowed
  let ( = ) _ _ = `Shadowed
end

module Fields = struct
  type ty =
    { x : int
    ; y : int [@key "some"]
    ; z : int [@key "some"]
    }
  [@@deriving yojson_fields ~capitalize:"PascalCase"]

  let%expect_test _ =
    print_s [%sexp (yojson_fields_of_ty : string list)];
    [%expect {| (X some some) |}]
  ;;
end

module Option = struct
  type ty =
    { x : int option option
    ; y : int option option
    }
  [@@deriving yojson]

  let%expect_test _ =
    let open Poly in
    let a = { x = None; y = Some None } in
    let b = yojson_of_ty a in
    let c = ty_of_yojson b in
    if None = c.x then Stdio.print_endline "x = None";
    if None = c.y then Stdio.print_endline "y = None";
    [%expect
      {|
      x = None
      y = None
      |}]
  ;;
end

module Default_omit = struct
  type ty =
    { x : int option
    ; y : int option [@default None] [@yojson_drop_default.equal]
    ; z : int [@default 0] [@yojson_drop_default.equal]
    ; b : int [@default 0] [@yojson_drop_default.equal]
    }
  [@@deriving yojson, equal]

  let ( = ) = equal_ty

  let%expect_test _ =
    let value = { x = None; y = None; z = 0; b = 1 } in
    let yojson = yojson_of_ty value in
    let yojson' = `Assoc [ "x", `Null; "y", `Null; "z", `Int 0; "b", `Int 1 ] in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (ty_of_yojson yojson = value);
    require (ty_of_yojson yojson' = value);
    [%expect {| (Assoc ((x Null) (b (Int 1)))) |}]
  ;;
end

module Tuple = struct
  type poly = int * float * string [@@deriving yojson, equal]

  let ( = ) = equal_poly

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_poly value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (poly_of_yojson yojson = value))
      [ 1, 1., "string"; 1, 2., "example" ];
    [%expect
      {|
      (List (
        (Int    1)
        (Float  1)
        (String string)))
      (List (
        (Int    1)
        (Float  2)
        (String example)))
      |}]
  ;;
end

module Types = struct
  type t = int * int32 * int64 * bool * int ref * nativeint * bytes * char * unit * float
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_t value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (t_of_yojson yojson = value))
      [ ( 1
        , Int32.of_int_exn 1
        , Int64.of_int 1
        , true
        , ref 1
        , Nativeint.of_int 1
        , Bytes.of_string "baddecaf"
        , 'c'
        , ()
        , 1. )
      ];
    [%expect
      {|
      (List (
        (Int    1)
        (Intlit 1)
        (Intlit 1)
        (Bool   true)
        (Int    1)
        (Intlit 1)
        (String baddecaf)
        (String c)
        Null
        (Float 1)))
      |}]
  ;;

  type lt = int lazy_t [@@deriving yojson]

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_lt value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (Yojson.Safe.equal (yojson_of_lt value) yojson);
        require Poly.(lt_of_yojson yojson = value))
      [ lazy 1 ];
    [%expect {| (Int 1) |}]
  ;;

  type opt = int option [@@deriving yojson, equal]

  let ( = ) = equal_opt

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_opt value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (Yojson.Safe.equal (yojson_of_opt value) yojson);
        require (opt_of_yojson yojson = value))
      [ Some 1; None ];
    [%expect
      {|
      (Int 1)
      Null
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = [%yojson_of: int list] value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require Poly.([%yojson_of: int list] value = yojson);
        require Poly.([%of_yojson: int list] yojson = value))
      [ []; [ 1 ]; [ 1; 2 ] ];
    [%expect
      {|
      (List ())
      (List ((Int 1)))
      (List (
        (Int 1)
        (Int 2)))
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = [%yojson_of: int array] value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require Poly.([%of_yojson: int array] yojson = value))
      [ [||]; [| 1 |]; [| 1; 2 |] ];
    [%expect
      {|
      (List ())
      (List ((Int 1)))
      (List (
        (Int 1)
        (Int 2)))
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun yojson ->
        let value = [%of_yojson: float] yojson in
        print_s ([%sexp_of: float] value);
        require Poly.([%of_yojson: float] yojson = value))
      [ `Float 1.; `Int 1; `Intlit "1" ];
    [%expect
      {|
      1
      1
      1
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun yojson ->
        let value = [%of_yojson: int32] yojson in
        print_s ([%sexp_of: int32] value);
        require Poly.([%of_yojson: int32] yojson = value))
      [ `Int 1; `Intlit "1" ];
    [%expect
      {|
      1
      1
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun yojson ->
        let value = [%of_yojson: int64] yojson in
        print_s ([%sexp_of: int64] value);
        require Poly.([%of_yojson: int64] yojson = value))
      [ `Int 1; `Intlit "1" ];
    [%expect
      {|
      1
      1
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun yojson ->
        let value = [%of_yojson: nativeint] yojson in
        print_s ([%sexp_of: nativeint] value);
        require Poly.([%of_yojson: nativeint] yojson = value))
      [ `Int 1; `Intlit "1" ];
    [%expect
      {|
      1
      1
      |}]
  ;;

  let%expect_test _ =
    let open Stdlib in
    let tbl = Hashtbl.create 10 in
    let _ = Hashtbl.add tbl "key_1" "value_1" in
    let _ = Hashtbl.add tbl "key_2" "value_2" in
    let _ = Hashtbl.add tbl "key_3" "value_3" in
    let yojson = [%yojson_of: (string, string) hashtbl] tbl in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require ([%of_yojson: (string, string) hashtbl] yojson = tbl);
    [%expect
      {|
      (List (
        (List ((String key_1) (String value_1)))
        (List ((String key_2) (String value_2)))
        (List ((String key_3) (String value_3)))))
      |}]
  ;;
end

module Sum_and_polymorphic_variants = struct
  type poly =
    [ `No_arg
    | `No_arg_with_renaming [@name "zero_arg"]
    | `One_arg of int
    | `One_arg_with_renaming of int [@name "one_arg"]
    | `One_tuple of int * string
    | `Two_args of int * string
    ]
  [@@deriving yojson, equal]

  let ( = ) = equal_poly

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_poly value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (poly_of_yojson yojson = value))
      [ `No_arg
      ; `No_arg_with_renaming
      ; `One_arg 1
      ; `One_arg_with_renaming 1
      ; `One_tuple (1, "a")
      ; `Two_args (1, "a")
      ];
    [%expect
      {|
      (List ((String No_arg)))
      (List ((String zero_arg)))
      (List (
        (String One_arg)
        (Int    1)))
      (List (
        (String one_arg)
        (Int    1)))
      (List (
        (String One_tuple)
        (Int    1)
        (String a)))
      (List (
        (String Two_args)
        (Int    1)
        (String a)))
      |}]
  ;;

  type nominal =
    | No_arg
    | One_arg of int
    | One_tuple of (int * string)
    | Two_args of int * string
  [@@deriving yojson, equal]

  let ( = ) = equal_nominal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_nominal value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (nominal_of_yojson yojson = value))
      [ No_arg; One_arg 1; One_tuple (1, "a"); Two_args (1, "a") ];
    [%expect
      {|
      (List ((String No_arg)))
      (List (
        (String One_arg)
        (Int    1)))
      (List (
        (String One_tuple)
        (List (
          (Int    1)
          (String a)))))
      (List (
        (String Two_args)
        (Int    1)
        (String a)))
      |}]
  ;;
end

module Name = struct
  type nominal =
    | Con_1 [@name "Name_1"]
    | Con_2 of int [@name "Name_2"]
    | Con_3 of (int * string) [@name "Name_3"]
    | Con_4 of int * string [@name "Name_4"]
    | Con_5 of { a : int } [@name "name_5"]
    | Con_6 of { b : int } [@name ""]
  [@@deriving yojson, equal]

  let ( = ) = equal_nominal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_nominal value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (nominal_of_yojson yojson = value))
      [ Con_1; Con_2 2; Con_3 (1, "a"); Con_4 (1, "a"); Con_5 { a = 1 }; Con_6 { b = 1 } ];
    [%expect
      {|
      (List ((String Name_1)))
      (List (
        (String Name_2)
        (Int    2)))
      (List (
        (String Name_3)
        (List (
          (Int    1)
          (String a)))))
      (List (
        (String Name_4)
        (Int    1)
        (String a)))
      (List ((String name_5) (Assoc ((a (Int 1))))))
      (List ((String "") (Assoc ((b (Int 1))))))
      |}]
  ;;
end

module Capitalize_variants = struct
  type nominal =
    | Con_1
    | Con_2 of int
    | Con_3 of (int * string)
    | Con_4 of int * string
    | Con_5 of { a : int }
    | Con_6 of { b : int } [@name "something-custom"]
  [@@deriving yojson ~capitalize:"kebab-case", equal]

  let ( = ) = equal_nominal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let yojson = yojson_of_nominal value in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require (nominal_of_yojson yojson = value))
      [ Con_1; Con_2 2; Con_3 (1, "a"); Con_4 (1, "a"); Con_5 { a = 1 }; Con_6 { b = 1 } ];
    [%expect
      {|
      (List ((String con-1)))
      (List (
        (String con-2)
        (Int    2)))
      (List (
        (String con-3)
        (List (
          (Int    1)
          (String a)))))
      (List (
        (String con-4)
        (Int    1)
        (String a)))
      (List ((String con-5) (Assoc ((a (Int 1))))))
      (List ((String something-custom) (Assoc ((b (Int 1))))))
      |}]
  ;;
end

module Records = struct
  type t =
    { a : int
    ; b : (float * string) list option
    }
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let t = { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let yojson = yojson_of_t t in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = t);
    [%expect
      {|
      (Assoc (
        (a (Int 2))
        (b (
          List (
            (List ((Float 1)   (String a)))
            (List ((Float 2.3) (String b))))))))
      |}]
  ;;
end

module Keys = struct
  type t =
    { name_a : int [@key "key_a"]
    ; name_b : int option [@key "key_b"]
    ; name_c : int option [@key "key_c"] [@yojson.option]
    ; name_d : int option [@key "key_d"] [@default None] [@yojson_drop_default.equal]
    ; name_e : int [@key ""]
    }
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let t =
      { name_a = 1; name_b = Some 2; name_c = Some 3; name_d = Some 4; name_e = 5 }
    in
    let yojson = yojson_of_t t in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = t);
    [%expect
      {|
      (Assoc (
        (key_a (Int 1))
        (key_b (Int 2))
        (key_c (Int 3))
        (key_d (Int 4))
        (""    (Int 5))))
      |}]
  ;;
end

module Capitalize_arg = struct
  type t =
    { name_a : int
    ; name_b : int option [@key "otherName"]
    }
  [@@deriving yojson ~capitalize:"camelCase", equal]

  let ( = ) = equal

  let%expect_test _ =
    let t = { name_a = 1; name_b = Some 2 } in
    let yojson = yojson_of_t t in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = t);
    [%expect
      {|
      (Assoc (
        (nameA     (Int 1))
        (otherName (Int 2))))
      |}]
  ;;
end

module Inline_records = struct
  type t =
    | A of
        { a : int
        ; b : (float * string) list option
        }
    | B of int
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let t = A { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let yojson = yojson_of_t t in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = t);
    [%expect
      {|
      (List (
        (String A)
        (Assoc (
          (a (Int 2))
          (b (
            List (
              (List ((Float 1)   (String a)))
              (List ((Float 2.3) (String b))))))))))
      |}]
  ;;

  let%expect_test _ =
    let t = B 100 in
    let yojson = yojson_of_t t in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = t);
    [%expect
      {|
      (List (
        (String B)
        (Int    100)))
      |}]
  ;;
end

module User_specified_conversion = struct
  type my_float = float

  let yojson_of_my_float n = `Float n
  let my_float_of_yojson = float_of_yojson

  let%expect_test _ =
    let my_float : my_float = 1.2 in
    let yojson = yojson_of_my_float my_float in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require Float.(my_float_of_yojson yojson = my_float);
    [%expect {| (Float 1.2) |}]
  ;;
end

module Abstract_types_are_allowed_in_structures : sig
  type t [@@deriving yojson]
end = struct
  type t [@@deriving yojson]
end

module Manifest_types = struct
  type a = { t : int }
  type b = a = { t : int } [@@deriving yojson]
end

module Function_types : sig
  type t1 = int -> unit [@@deriving yojson]
  type t2 = label:int -> ?optional:int -> unit -> unit [@@deriving yojson]
end = struct
  type t1 = int -> unit [@@deriving yojson]
  type t2 = label:int -> ?optional:int -> unit -> unit [@@deriving yojson]
end

module No_unused_rec = struct
  type r = { r : int } [@@deriving yojson]
end

module Field_name_should_not_be_rewritten = struct
  open No_unused_rec

  type nonrec r = { r : r }

  let _ = fun (r : r) -> r.r
end

module Polymorphic_variant_inclusion = struct
  type sub1 =
    [ `C1
    | `C2
    ]
  [@@deriving yojson, equal]

  type 'b sub2 =
    [ `C4
    | `C5 of 'b
    ]
  [@@deriving yojson, equal]

  type ('a, 'b) t = [ sub1 | `C3 of [ `Nested of 'a ] | 'b sub2 | `C6 ] option
  [@@deriving yojson, equal]

  let%expect_test _ =
    let cases : (string * string, float) t list =
      [ None
      ; Some `C1
      ; Some `C2
      ; Some (`C3 (`Nested ("a", "b")))
      ; Some `C4
      ; Some (`C5 1.5)
      ; Some `C6
      ]
    in
    List.iter
      ~f:(fun t ->
        let yojson = [%yojson_of: (string * string, float) t] t in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require
          ([%equal: (string * string, float) t]
             ([%of_yojson: (string * string, float) t] yojson)
             t))
      cases;
    [%expect
      {|
      Null
      (List ((String C1)))
      (List ((String C2)))
      (List (
        (String C3)
        (List (
          (String Nested)
          (List (
            (String a)
            (String b)))))))
      (List ((String C4)))
      (List (
        (String C5)
        (Float  1.5)))
      (List ((String C6)))
      |}]
  ;;

  type sub1_alias = sub1 [@@deriving yojson_poly, equal]

  type u =
    [ `A
    | sub1_alias
    | `D
    ]
  [@@deriving yojson, equal]

  let ( = ) = equal_u

  let%expect_test _ =
    let cases : u list = [ `A; `C1; `C2; `D ] in
    List.iter
      ~f:(fun u ->
        let yojson = [%yojson_of: u] u in
        print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
        require ([%of_yojson: u] yojson = u))
      cases;
    [%expect
      {|
      (List ((String A)))
      (List ((String C1)))
      (List ((String C2)))
      (List ((String D)))
      |}]
  ;;
end

module Polymorphic_record_field = struct
  type 'x t =
    { poly : 'a 'b. 'a list
    ; maybe_x : 'x option
    }
  [@@deriving yojson]

  let%expect_test _ =
    let t x = { poly = []; maybe_x = Some x } in
    let yojson = yojson_of_t yojson_of_int (t 1) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require Poly.(t_of_yojson int_of_yojson yojson = t 1);
    require Poly.(yojson_of_t yojson_of_int (t 1) = yojson);
    [%expect {| (Assoc ((poly (List ())) (maybe_x (Int 1)))) |}]
  ;;
end

module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] [@@deriving yojson]
  end = struct
    type t = [ `A ] [@@deriving yojson]
  end

  include struct
    [@@@warning "-unused-module"]

    module Empty = struct end
  end

  module No_warning2 (_ : sig
      type t [@@deriving yojson]
    end) =
  struct end

  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with yojson end) = Empty *)
  module type S = sig
    type t = [ `A ] [@@deriving yojson]
  end

  module No_warning4 : S = struct
    type t = [ `A ] [@@deriving yojson]
  end

  module No_warning5 : S = (
    (
    struct
      type t = [ `A ] [@@deriving yojson]
    end :
      S) :
      S)

  module Nested_functors
      (_ : sig
         type t [@@deriving yojson]
       end)
      (_ : sig
         type t [@@deriving yojson]
       end) =
  struct end

  let () =
    let module M : sig
      type t [@@deriving yojson]
    end = struct
      type t [@@deriving yojson]
    end
    in
    ()
  ;;

  module Include = struct
    include (
    struct
      type t = int [@@deriving yojson]
    end :
      sig
        type t [@@deriving yojson]
      end
      with type t := int)
  end
end

module Default = struct
  type t = { a : int [@default 2] [@yojson_drop_default.equal] }
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let yojson = yojson_of_t { a = 1 } in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = { a = 1 });
    [%expect {| (Assoc ((a (Int 1)))) |}]
  ;;

  let%expect_test _ =
    let yojson = yojson_of_t { a = 2 } in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = { a = 2 });
    require (t_of_yojson (`Assoc [ "a", `Int 2 ]) = { a = 2 });
    [%expect {| (Assoc ()) |}]
  ;;
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the yojson_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = [ `A ] as 'a constraint 'a = a [@@deriving yojson, equal]
  end = struct
    type a = [ `A ] [@@deriving yojson, equal]
    type t = [ `A ] [@@deriving yojson, equal]
  end

  let ( = ) = B.equal

  let%expect_test _ =
    let yojson = B.yojson_of_t `A in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (`A = B.t_of_yojson yojson);
    [%expect {| (List ((String A))) |}]
  ;;

  module B2 = struct
    type t =
      [ B.t
      | `B
      ]
    [@@deriving yojson]
  end

  module C : sig
    type t = int as 'a [@@deriving yojson]
  end = struct
    type t = int [@@deriving yojson]
  end

  module D : sig
    type t = 'a constraint 'a = int [@@deriving yojson]
  end = struct
    type t = int [@@deriving yojson]
  end
end

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] [@@deriving yojson]
  type 'a u = [ t | `b of 'a ] * int [@@deriving yojson]
end

module Drop_default = struct
  open! Base
  open Expect_test_helpers_core

  type t = { a : int } [@@deriving equal]

  let test ?cr t_of_yojson yojson_of_t =
    let ( = ) : Yojson.t -> Yojson.t -> bool = Yojson.equal in
    require ?cr ((`Assoc [ "a", `Int 1 ] : Yojson.t) = yojson_of_t { a = 1 });
    require ?cr ((`Assoc [] : Yojson.t) = yojson_of_t { a = 2 });
    let ( = ) = equal in
    require ?cr (t_of_yojson (`Assoc [ "a", `Int 1 ] : Yojson.t) = { a = 1 });
    require ?cr (t_of_yojson (`Assoc [] : Yojson.t) = { a = 2 })
  ;;

  type my_int = int [@@deriving yojson]

  module Poly = struct
    type nonrec t = t = { a : my_int [@default 2] [@yojson_drop_default ( = )] }
    [@@deriving yojson]

    let%expect_test _ =
      test t_of_yojson yojson_of_t;
      [%expect {| |}]
    ;;
  end

  module Equal = struct
    let equal_my_int = equal_int

    type nonrec t = t = { a : my_int [@default 2] [@yojson_drop_default.equal] }
    [@@deriving yojson]

    let%expect_test _ =
      test t_of_yojson yojson_of_t;
      [%expect {| |}]
    ;;
  end

  module Compare = struct
    let compare_my_int = compare_int

    type nonrec t = t = { a : my_int [@default 2] [@yojson_drop_default.compare] }
    [@@deriving yojson]

    let%expect_test _ =
      test t_of_yojson yojson_of_t;
      [%expect {| |}]
    ;;
  end

  module Yojson = struct
    type nonrec t = t = { a : my_int [@default 2] [@yojson_drop_default.yojson] }
    [@@deriving yojson]

    let%expect_test _ =
      test t_of_yojson yojson_of_t;
      [%expect {| |}]
    ;;
  end
end

module Drop_if = struct
  type t = { a : int [@default 2] [@yojson_drop_if fun x -> Int.(x % 2 = 0)] }
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { a = 2 } in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    require (t_of_yojson (`Assoc [ "a", `Int 2 ]) = value);
    [%expect {| (Assoc ()) |}]
  ;;

  let%expect_test _ =
    let value = { a = 1 } in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    [%expect {| (Assoc ((a (Int 1)))) |}]
  ;;

  type u =
    { a : int
         [@yojson_drop_if
           fun x ->
             (* pa_type_conv used to drop parens altogether, causing type errors in the
                 following code *)
             let pair = x, 2 in
             match Some pair with
             | None -> true
             | Some (x, y) -> Poly.(x = y)]
    }
  [@@deriving yojson]
end

module Omit_nil = struct
  type natural_option = int [@@deriving equal]

  let yojson_of_natural_option i = if i >= 0 then yojson_of_int i else `Null

  let natural_option_of_yojson = function
    | `Null -> -1
    | yojson -> int_of_yojson yojson
  ;;

  type t = { a : natural_option [@default -1] [@yojson_drop_default.equal] }
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { a = 1 } in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    [%expect {| (Assoc ((a (Int 1)))) |}]
  ;;

  let%expect_test _ =
    let value = { a = -1 } in
    let yojson = yojson_of_t value in
    let yojson' = `Assoc [] in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    require (t_of_yojson yojson' = value);
    [%expect {| (Assoc ()) |}]
  ;;

  type t2 = A of { a : int option [@yojson.option] } [@@deriving yojson, equal]

  let ( = ) = equal_t2

  let%expect_test _ =
    let value = A { a = None } in
    let yojson = yojson_of_t2 value in
    let yojson' = `List [ `String "A"; `Assoc [] ] in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t2_of_yojson yojson = value);
    require (t2_of_yojson yojson' = value);
    [%expect {| (List ((String A) (Assoc ()))) |}]
  ;;

  let%expect_test _ =
    let value = A { a = Some 1 } in
    let yojson = yojson_of_t2 value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t2_of_yojson yojson = value);
    [%expect {| (List ((String A) (Assoc ((a (Int 1)))))) |}]
  ;;
end

module No_unused_rec_warning = struct
  type r = { field : r -> unit } [@@deriving yojson_of]
end

module True_and_false = struct
  type t =
    | True
    | False
  [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = True in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    [%expect {| (List ((String True))) |}]
  ;;

  let%expect_test _ =
    let value = False in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    [%expect {| (List ((String False))) |}]
  ;;

  type u =
    | True of int
    | False of int
  [@@deriving yojson, equal]

  let ( = ) = equal_u

  let%expect_test _ =
    let value = True 1 in
    let yojson = yojson_of_u value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (u_of_yojson yojson = value);
    [%expect
      {|
      (List (
        (String True)
        (Int    1)))
      |}]
  ;;

  let%expect_test _ =
    let value = False 0 in
    let yojson = yojson_of_u value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (u_of_yojson yojson = value);
    [%expect
      {|
      (List (
        (String False)
        (Int    0)))
      |}]
  ;;

  type v =
    [ `True
    | `False of int
    ]
  [@@deriving yojson, equal]

  let ( = ) = equal_v

  let%expect_test _ =
    let value = `True in
    let yojson = yojson_of_v value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (v_of_yojson yojson = value);
    [%expect {| (List ((String True))) |}]
  ;;

  let%expect_test _ =
    let value = `False 0 in
    let yojson = yojson_of_v value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (v_of_yojson yojson = value);
    [%expect
      {|
      (List (
        (String False)
        (Int    0)))
      |}]
  ;;
end

module Gadt = struct
  (* plain type without argument *)
  type 'a s = Packed : 'a s [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: int s] Packed in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect {| (List ((String Packed))) |}]
  ;;

  (* two kind of existential variables *)
  type 'a t = Packed : 'a * _ * ('b[@yojson.opaque]) -> 'a t [@warning "-3"]
  [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: int t] (Packed (2, "asd", 1.)) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect
      {|
      (List (
        (String Packed)
        (Int    2)
        (String _)
        (String <opaque>)))
      |}]
  ;;

  (* Safe.to_channel stderr ([%yojson_of: int t] (Packed (2, "asd", 1.))) *)
  (* plain type with argument *)
  type 'a u = A : 'a -> 'a u [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: int u] (A 2) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect
      {|
      (List (
        (String A)
        (Int    2)))
      |}]
  ;;

  (* recursive *)
  type v = A : v option -> v [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: v] (A (Some (A None))) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect {| (List ((String A) (List ((String A) Null)))) |}]
  ;;

  (* implicit existential variable *)
  type w = A : 'a * int * ('a -> string) -> w [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: w] (A (1., 2, Float.to_string)) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect
      {|
      (List (
        (String A)
        (String _)
        (Int    2)
        (String <fun>)))
      |}]
  ;;

  (* tricky variable naming *)
  type 'a x = A : 'a -> 'b x [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: int x] (A 1.) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect
      {|
      (List (
        (String A)
        (String _)))
      |}]
  ;;

  (* interaction with inline record *)
  type _ x2 = A : { x : 'c } -> 'c x2 [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: int x2] (A { x = 1 }) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect {| (List ((String A) (Assoc ((x (Int 1)))))) |}]
  ;;

  (* unused but colliding variables *)
  type (_, _) y = A : ('a, 'a) y [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: (int, int) y] A in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect {| (List ((String A))) |}]
  ;;

  (* making sure we're not reversing parameters *)
  type (_, _) z = A : ('a * 'b) -> ('a, 'b) z [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: (int, string) z] (A (1, "a")) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect
      {|
      (List (
        (String A)
        (List (
          (Int    1)
          (String a)))))
      |}]
  ;;

  (* interaction with universal quantifiers *)
  type _ z2 = A : { x : 'c. 'c option } -> 'c z2 [@@deriving yojson_of]

  let%expect_test _ =
    let yojson = [%yojson_of: unit z2] (A { x = None }) in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    [%expect {| (List ((String A) (Assoc ((x Null))))) |}]
  ;;
end

module Anonymous_variable = struct
  type _ t = int [@@deriving yojson]

  let%expect_test _ =
    let yojson = [%yojson_of: _ t] 2 in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require Poly.([%of_yojson: _ t] yojson = 2);
    [%expect {| (Int 2) |}]
  ;;

  (* making sure we don't generate signatures like (_ -> Safe.t) -> _ t -> Safe.t which
     are too general *)
  module M : sig
    type _ t [@@deriving yojson]
  end = struct
    type 'a t = 'a [@@deriving yojson]
  end
end

module Record_field_disambiguation = struct
  type a =
    { fl : float
    ; b : b
    }

  and b = { fl : int } [@@deriving yojson]
end

module Private = struct
  type t = private int [@@deriving yojson_of]
  type ('a, 'b) u = private t [@@deriving yojson_of]
  type ('a, 'b, 'c) v = private ('a, 'b) u [@@deriving yojson_of]
end

module Nonregular_types = struct
  type 'a nonregular =
    | Leaf of 'a
    | Branch of ('a * 'a) nonregular
  [@@deriving yojson]

  type 'a variant = [ `A of 'a ] [@@deriving yojson]

  type ('a, 'b) nonregular_with_variant =
    | Branch of ([ | 'a list variant ], 'b) nonregular_with_variant
  [@@deriving yojson]
end

module Opaque = struct
  type t = (int[@yojson.opaque]) list [@@deriving yojson]

  let%expect_test _ =
    let value = [ 1; 2 ] in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require_does_raise (fun () -> t_of_yojson yojson);
    [%expect
      {|
      (List (
        (String <opaque>)
        (String <opaque>)))
      (Of_yojson_error
       "opaque_of_yojson: cannot convert opaque values"
       "\"<opaque>\"")
      |}]
  ;;

  type u = ([ `A of int ][@yojson.opaque]) [@@deriving yojson]

  let%expect_test _ =
    let value = `A 1 in
    let yojson = yojson_of_u value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require_does_raise (fun () -> u_of_yojson yojson);
    [%expect
      {|
      (String <opaque>)
      (Of_yojson_error
       "opaque_of_yojson: cannot convert opaque values"
       "\"<opaque>\"")
      |}]
  ;;
end

module Optional = struct
  type t = { optional : int option [@yojson.option] } [@@deriving yojson, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { optional = None } in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    [%expect {| (Assoc ()) |}]
  ;;

  let%expect_test _ =
    let value = { optional = Some 5 } in
    let yojson = yojson_of_t value in
    print_s (Yojson.Safe.Alternate_sexp.sexp_of_t yojson);
    require (t_of_yojson yojson = value);
    [%expect {| (Assoc ((optional (Int 5)))) |}]
  ;;
end

module Variance = struct
  type (+'a, -'b, 'c, +_, -_, _) t [@@deriving yojson]
end

module Clash = struct
  (* Same name for type-var and type-name; must be careful when introducing rigid type names. *)
  type 'hey hey = Hey of 'hey [@@deriving yojson]
  type 'hey rigid_hey = Hey of 'hey [@@deriving yojson]
  type ('foo, 'rigid_foo) foo = Foo of 'foo [@@deriving yojson]
  type 'rigid_bar rigid_rigid_bar = Bar [@@deriving yojson]
end

module Applicative_functor_types = struct
  module Bidirectional_map = struct
    type ('k1, 'k2) t

    module S
        (K1 : sig
           type t
         end)
        (K2 : sig
           type t
         end) =
    struct
      type nonrec t = (K1.t, K2.t) t
    end

    module type Of_yojsonable = sig
      type t [@@deriving of_yojson]
    end

    let s__t_of_yojson
      (type k1 k2)
      (module _ : Of_yojsonable with type t = k1)
      (module _ : Of_yojsonable with type t = k2)
      (_ : Yojson.t)
      : (k1, k2) t
      =
      assert false
    ;;
  end

  module Int = struct
    type t = int [@@deriving of_yojson]
  end

  module String = struct
    type t = string [@@deriving of_yojson]
  end

  module M : sig
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_yojson]
  end = struct
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_yojson]
  end
end

module Type_extensions = struct
  let (_ : [%yojson_of: int]) = [%yojson_of: int]
  let (_ : [%of_yojson: int]) = [%of_yojson: int]
end

module Allow_extra_fields = struct
  module M1 = struct
    type t1 = { a : int } [@@deriving yojson, equal]
    type t2 = t1 = { a : int } [@@deriving yojson, equal] [@@yojson.allow_extra_fields]

    let ( = ) = equal_t2

    let%expect_test _ =
      let yojson = Yojson.from_string {|{"a":1}|} in
      let yojson_extra = Yojson.from_string {|{"a":1,"b":2}|} in
      require (t2_of_yojson yojson = t2_of_yojson yojson_extra);
      require (t1_of_yojson yojson = t2_of_yojson yojson);
      require_does_raise (fun () -> t1_of_yojson yojson_extra);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Allow_extra_fields.M1.t1_of_yojson: extra fields: b"
         "{\"a\":1,\"b\":2}")
        |}]
    ;;
  end

  module M2 = struct
    type t1 = A of { a : int list } [@@deriving yojson, equal]

    type t2 = t1 = A of { a : int list } [@yojson.allow_extra_fields]
    [@@deriving yojson, equal]

    let ( = ) = equal_t2

    let%expect_test _ =
      let yojson = Yojson.from_string {|["A",{"a":[0]}]|} in
      let yojson_extra = Yojson.from_string {|["A",{"a":[0],"b":"1"}]|} in
      require (t2_of_yojson yojson = t2_of_yojson yojson_extra);
      require (t1_of_yojson yojson = t2_of_yojson yojson);
      require_does_raise (fun () -> t1_of_yojson yojson_extra);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Allow_extra_fields.M2.t1_of_yojson: extra fields: b"
         "[\"A\",{\"a\":[0],\"b\":\"1\"}]")
        |}]
    ;;
  end
end

module Exceptions = struct
  module Variant = struct
    type t =
      | A [@name "AA"]
      | B of int
      | C of { a : int }
      | D of int * string
    [@@deriving yojson]

    let%expect_test _ =
      let wrong_constr_name = `List [ `String "Z" ] in
      require_does_raise (fun () -> t_of_yojson wrong_constr_name);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Variant.t_of_yojson: unexpected variant constructor"
         "[\"Z\"]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_constr_name = `List [ `String "A" ] in
      require_does_raise (fun () -> t_of_yojson wrong_constr_name);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Variant.t_of_yojson: unexpected variant constructor"
         "[\"A\"]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_constr_name = `List [ `String "AA" ] in
      require_does_not_raise (fun () -> Base.ignore (t_of_yojson wrong_constr_name));
      [%expect {| |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "B"; `Float 1. ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect {| (Of_yojson_error "int_of_yojson: integer needed" 1.0) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "B"; `Intlit "1" ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect {| (Of_yojson_error "int_of_yojson: integer needed" 1) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "C"; `String "string" ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Variant.t_of_yojson: unexpected variant constructor"
         "[\"C\",\"string\"]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "C"; `Assoc [ "b", `Int 1 ] ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Variant.t_of_yojson: extra fields: b"
         "[\"C\",{\"b\":1}]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "D"; `Int 1; `Float 1. ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect {| (Of_yojson_error "string_of_yojson: string needed" 1.0) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num = `List [ `String "D"; `List [ `Int 1; `Float 1. ] ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_num);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Variant.t_of_yojson: sum tag \"D\" has incorrect number of arguments"
         "[\"D\",[1,1.0]]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num = `List [ `String "D"; `Int 1; `Float 1.; `String "str" ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_num);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Variant.t_of_yojson: sum tag \"D\" has incorrect number of arguments"
         "[\"D\",1,1.0,\"str\"]")
        |}]
    ;;
  end

  module Sum = struct
    type t =
      [ `A
      | `B of int
      | `D of int * string
      ]
    [@@deriving yojson]

    let%expect_test _ =
      let wrong_constr_name = `List [ `String "Z" ] in
      require_does_raise (fun () -> t_of_yojson wrong_constr_name);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Sum.t_of_yojson: no matching variant found"
         "[\"Z\"]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "B"; `Float 1. ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect {| (Of_yojson_error "int_of_yojson: integer needed" 1.0) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "B"; `Intlit "1" ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect {| (Of_yojson_error "int_of_yojson: integer needed" 1) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `List [ `String "D"; `Int 1; `Float 1. ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_type);
      [%expect {| (Of_yojson_error "string_of_yojson: string needed" 1.0) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num = `List [ `String "D"; `List [ `Int 1; `Float 1. ] ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_num);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Sum.t_of_yojson: polymorphic variant tag \"D\" has incorrect number of arguments"
         "[\"D\",[1,1.0]]")
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num = `List [ `String "D"; `Int 1; `Float 1.; `String "str" ] in
      require_does_raise (fun () -> t_of_yojson wrong_arg_num);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Sum.t_of_yojson: polymorphic variant tag \"D\" has incorrect number of arguments"
         "[\"D\",1,1.0,\"str\"]")
        |}]
    ;;
  end

  module Record = struct
    type t =
      { a : int [@key "A"]
      ; b : string
      ; c : float
      ; d : int option
      ; e : int option [@default None]
      ; f : int [@default 0]
      }
    [@@deriving yojson]

    let%expect_test _ =
      let wrong_field_name =
        `Assoc
          [ "a", `Int 1
          ; "b", `String "str"
          ; "c", `Float 1.
          ; "d", `Null
          ; "e", `Int 1
          ; "f", `Int 1
          ]
      in
      require_does_raise (fun () -> t_of_yojson wrong_field_name);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Record.t_of_yojson: extra fields: a"
         "{\"a\":1,\"b\":\"str\",\"c\":1.0,\"d\":null,\"e\":1,\"f\":1}")
        |}]
    ;;

    let%expect_test _ =
      let wrong_field_type =
        `Assoc
          [ "A", `String "A"
          ; "b", `String "str"
          ; "c", `Float 1.
          ; "d", `Null
          ; "e", `Int 1
          ; "f", `Int 1
          ]
      in
      require_does_raise (fun () -> t_of_yojson wrong_field_type);
      [%expect {| (Of_yojson_error "int_of_yojson: integer needed" "\"A\"") |}]
    ;;

    let%expect_test _ =
      let wrong_field_number =
        `Assoc [ "A", `Int 1; "b", `String "str"; "c", `Float 1. ]
      in
      require_does_raise (fun () -> t_of_yojson wrong_field_number);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Record.t_of_yojson: the following record elements were undefined: d"
         "{\"A\":1,\"b\":\"str\",\"c\":1.0}")
        |}]
    ;;

    let%expect_test _ =
      let duplicate_fields =
        `Assoc
          [ "A", `Int 1
          ; "b", `String "str"
          ; "c", `Float 1.
          ; "d", `Null
          ; "e", `Int 1
          ; "f", `Int 1
          ; "f", `Int 1
          ]
      in
      require_does_raise (fun () -> t_of_yojson duplicate_fields);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Record.t_of_yojson: duplicate fields: f"
         "{\"A\":1,\"b\":\"str\",\"c\":1.0,\"d\":null,\"e\":1,\"f\":1,\"f\":1}")
        |}]
    ;;

    let%expect_test _ =
      let extra_fields =
        `Assoc
          [ "A", `Int 1
          ; "b", `String "str"
          ; "c", `Float 1.
          ; "d", `Null
          ; "e", `Int 1
          ; "f", `Int 1
          ; "g", `Int 1
          ]
      in
      require_does_raise (fun () -> t_of_yojson extra_fields);
      [%expect
        {|
        (Of_yojson_error
         "ppx_yojson_test.ml.Exceptions.Record.t_of_yojson: extra fields: g"
         "{\"A\":1,\"b\":\"str\",\"c\":1.0,\"d\":null,\"e\":1,\"f\":1,\"g\":1}")
        |}]
    ;;
  end
end
