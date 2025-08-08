open Base
open Ppxlib
open Ast_builder.Default
module Attrs = Attrs

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

(* Simplifies match cases, for readability of the generated code. It's not obvious we can
   stick this in ppx_core, as (match e1 with p -> e2) and (let p = e1 in e2) are not typed
   exactly the same (type inference goes in different order, meaning type disambiguation
   differs). *)
let pexp_match ~loc expr cases =
  match cases with
  | [ { pc_lhs; pc_guard = None; pc_rhs } ] ->
    (match pc_lhs, expr with
     | ( { ppat_attributes = []; ppat_desc = Ppat_var { txt = ident; _ }; _ }
       , { pexp_attributes = []; pexp_desc = Pexp_ident { txt = Lident ident'; _ }; _ } )
       when String.equal ident ident' -> pc_rhs
     | _ -> pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat:pc_lhs ~expr ] pc_rhs)
  | _ -> pexp_match ~loc expr cases
;;

let get_label_name (ld : label_declaration) ~capitalization =
  match Attribute.get Attrs.yojson_key ld with
  | Some custom -> custom
  | None ->
    let name = ld.pld_name.txt in
    (match capitalization with
     | None -> name
     | Some c -> Capitalization_ppx_configuration.apply_to_snake_case_exn c name)
;;

module Fun_or_match = struct
  type t =
    | Fun of expression
    | Match of case list

  let expr ~loc t =
    match t with
    | Fun f -> f
    | Match cases -> pexp_function ~loc cases
  ;;

  let unroll ~loc e t =
    match t with
    | Fun f -> eapply ~loc f [ e ]
    | Match cases -> pexp_match ~loc e cases
  ;;

  let map_tmp_vars ~loc ts =
    let vars = List.mapi ts ~f:(fun i _ -> "v" ^ Int.to_string i) in
    let bindings =
      List.map2_exn vars ts ~f:(fun var t ->
        let expr = unroll ~loc (evar ~loc var) t in
        value_binding ~loc ~pat:(pvar ~loc var) ~expr)
    in
    bindings, List.map vars ~f:(pvar ~loc), List.map vars ~f:(evar ~loc)
  ;;
end

(* A renaming is a mapping from type variable name to type variable name.
   In definitions such as:

   type 'a t =
   | A : <type> -> 'b t
   | B of 'a

   we generate a function that takes an yojson_of parameter named after 'a, but 'a is not in
   scope in <type> when handling the constructor A (because A is a gadt constructor).
   Instead the type variables in scope are the ones defined in the return type of A,
   namely 'b. There could be less or more type variable in cases such as:

   type _ less = Less : int less
   type _ more = More : ('a * 'a) more

   If for instance, <type> is ['b * 'c], when we find 'b, we will look for ['b] in the
   renaming and find ['a] (only in that gadt branch, it could be something else in other
   branches), at which point we can call the previously bound yojson_of parameter named
   after 'a.
   If we can't find a resulting name, like when looking up ['c] in the renaming, then we
   assume the variable is existentially quantified and treat it as [_] (which is ok,
   assuming there are no constraints). *)
module Renaming : sig
  type t

  val identity : t
  val add_universally_bound : t -> string loc * Ppxlib_jane.jkind_annotation option -> t

  type binding_kind =
    | Universally_bound of string
    | Existentially_bound

  val binding_kind : t -> string -> binding_kind
  val of_gadt : string list -> constructor_declaration -> t
end = struct
  type error = string Loc.t
  type t = (string, error) Result.t Map.M(String).t option

  let identity = None

  type binding_kind =
    | Universally_bound of string
    | Existentially_bound

  let add_universally_bound (t : t) (name, _) : t =
    let name = name.txt in
    match t with
    | None -> None
    | Some map -> Some (Map.set ~key:name ~data:(Ok name) map)
  ;;

  let binding_kind t var =
    match t with
    | None -> Universally_bound var
    | Some map ->
      (match Map.find map var with
       | None -> Existentially_bound
       | Some (Ok value) -> Universally_bound value
       | Some (Error { loc; txt }) -> Location.raise_errorf ~loc "%s" txt)
  ;;

  (* Return a map translating type variables appearing in the return type of a GADT
     constructor to their name in the type parameter list.

     For instance:

     {[
       type ('a, 'b) t = X : 'x * 'y -> ('x, 'y) t
     ]}

     will produce:

     {[
       "x" -> Ok "a"
                "y" -> Ok "b"
     ]}

     If a variable appears twice in the return type it will map to [Error _]. If a
     variable cannot be mapped to a parameter of the type declaration, it will map to
     [Error] (for instance [A : 'a -> 'a list t]).

     It returns None on user error, to let the typer give the error message *)
  let of_gadt =
    (* Add all type variables of a type to a map. *)
    let add_typevars =
      object
        inherit [(string, error) Result.t Map.M(String).t] Ast_traverse.fold as super

        method! core_type ty map =
          match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
          | Ptyp_var (var, _) ->
            let error =
              { loc = ty.ptyp_loc
              ; txt =
                  "ppx_yojson_conv: variable is not a parameter of the type constructor"
              }
            in
            Map.set map ~key:var ~data:(Error error)
          | _ -> super#core_type ty map
      end
    in
    let aux map tp_name tp_in_return_type =
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree tp_in_return_type.ptyp_desc with
      | Ptyp_var (var, _) ->
        let data =
          if Map.mem map var
          then (
            let loc = tp_in_return_type.ptyp_loc in
            Error { loc; txt = "ppx_yojson_conv: duplicate variable" })
          else Ok tp_name
        in
        Map.set map ~key:var ~data
      | _ -> add_typevars#core_type tp_in_return_type map
    in
    fun tps cd ->
      match cd.pcd_res with
      | None -> None
      | Some ty ->
        (match ty.ptyp_desc with
         | Ptyp_constr (_, params) ->
           if List.length params <> List.length tps
           then None
           else
             Some
               (Stdlib.ListLabels.fold_left2
                  tps
                  params
                  ~init:(Map.empty (module String))
                  ~f:aux)
         | _ -> None)
  ;;
end

(* Utility functions *)

let replace_variables_by_underscores =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type_desc t =
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree t with
        | Ptyp_var (_, jkind) ->
          Ppxlib_jane.Shim.Core_type_desc.to_parsetree (Ptyp_any jkind)
        | _ -> super#core_type_desc t
    end
  in
  map#core_type
;;

let rigid_type_var ~type_name x =
  let prefix = "rigid_" in
  if String.equal x type_name || String.is_prefix x ~prefix
  then prefix ^ x ^ "_of_type_" ^ type_name
  else x
;;

let make_type_rigid ~type_name =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type ty =
        let ptyp_desc =
          match Ppxlib_jane.Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
          | Ptyp_var (s, _) ->
            Ptyp_constr (Located.lident ~loc:ty.ptyp_loc (rigid_type_var ~type_name s), [])
          | _ -> super#core_type_desc ty.ptyp_desc
        in
        { ty with ptyp_desc }
    end
  in
  map#core_type
;;

(* Generates the quantified type [ ! 'a .. 'z . (make_mono_type t ('a .. 'z)) ] or
   [type a .. z. make_mono_type t (a .. z)] when [use_rigid_variables] is true.
   Annotation are needed for non regular recursive datatypes and gadt when the return type
   of constructors are constrained. Unfortunately, putting rigid variables everywhere does
   not work because of certains types with constraints. We thus only use rigid variables
   for sum types, which includes all GADTs. *)

let tvars_of_core_type : core_type -> string list =
  let tvars =
    object
      inherit [string list] Ast_traverse.fold as super

      method! core_type x acc =
        match Ppxlib_jane.Shim.Core_type_desc.of_parsetree x.ptyp_desc with
        | Ptyp_var (x, _) -> if List.mem acc x ~equal:String.equal then acc else x :: acc
        | _ -> super#core_type x acc
    end
  in
  fun typ -> List.rev (tvars#core_type typ [])
;;

let constrained_function_binding
  (* placing a suitably polymorphic or rigid type constraint on the pattern or body *)
    (loc : Location.t)
  (td : type_declaration)
  (typ : core_type)
  ~(tps : string loc list)
  ~(func_name : string)
  (body : expression)
  =
  let vars = tvars_of_core_type typ in
  let has_vars =
    match vars with
    | [] -> false
    | _ :: _ -> true
  in
  let pat =
    let pat = pvar ~loc func_name in
    if not has_vars
    then pat
    else (
      let vars = List.map ~f:(fun txt -> { txt; loc }) vars in
      ppat_constraint ~loc pat (ptyp_poly ~loc vars typ))
  in
  let body =
    let use_rigid_variables =
      match td.ptype_kind with
      | Ptype_variant _ -> true
      | _ -> false
    in
    if use_rigid_variables
    then (
      let type_name = td.ptype_name.txt in
      List.fold_right
        tps
        ~f:(fun tp body ->
          pexp_newtype ~loc { txt = rigid_type_var ~type_name tp.txt; loc = tp.loc } body)
        ~init:(pexp_constraint ~loc body (make_type_rigid ~type_name typ)))
    else if has_vars
    then body
    else pexp_constraint ~loc body typ
  in
  value_binding ~loc ~pat ~expr:body
;;

let really_recursive rec_flag tds =
  (object
     inherit type_is_recursive rec_flag tds as super

     method! core_type ctype =
       match ctype with
       | _ when Option.is_some (Attribute.get ~mark_as_seen:false Attrs.opaque ctype) ->
         ()
       | [%type: [%t? _] yojson_opaque] -> ()
       | _ -> super#core_type ctype
  end)
    #go
    ()
;;

(* Generates the signature for type conversion to Yojsons *)
module Sig_generate_yojson_of = struct
  let type_of_yojson_of ~loc t = [%type: [%t t] -> Ppx_yojson_conv_lib.Yojson.Safe.t]
  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_yojson_of

  let mk_sig ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      psig_value
        ~loc
        (value_description
           ~loc
           ~name:(Located.map (fun x -> "yojson_of_" ^ x) td.ptype_name)
           ~type_:(mk_type td)
           ~prim:[]))
  ;;
end

(* Generates the signature for type conversion from Yojsons *)
module Sig_generate_of_yojson = struct
  let type_of_of_yojson ~loc t = [%type: Ppx_yojson_conv_lib.Yojson.Safe.t -> [%t t]]
  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_of_yojson

  let sig_of_td with_poly td =
    let of_yojson_type = mk_type td in
    let loc = { td.ptype_loc with loc_ghost = true } in
    let of_yojson_item =
      psig_value
        ~loc
        (value_description
           ~loc
           ~name:(Located.map (fun s -> s ^ "_of_yojson") td.ptype_name)
           ~type_:of_yojson_type
           ~prim:[])
    in
    match with_poly, is_polymorphic_variant td ~sig_:true with
    | true, `Surely_not ->
      Location.raise_errorf
        ~loc
        "Sig_generate_of_yojson.sig_of_td: yojson_poly annotation but type is surely not \
         a polymorphic variant"
    | false, (`Surely_not | `Maybe) -> [ of_yojson_item ]
    | (true | false), `Definitely | true, `Maybe ->
      [ of_yojson_item
      ; psig_value
          ~loc
          (value_description
             ~loc
             ~name:(Located.map (fun s -> "__" ^ s ^ "_of_yojson__") td.ptype_name)
             ~type_:of_yojson_type
             ~prim:[])
      ]
  ;;

  let mk_sig ~poly ~loc:_ ~path:_ (_rf, tds) = List.concat_map tds ~f:(sig_of_td poly)
end

module Str_generate_yojson_of = struct
  (* Handling of record defaults *)

  let yojson_of_type_constr ~loc id args =
    type_constr_conv ~loc id ~f:(fun s -> "yojson_of_" ^ s) args
  ;;

  (* Conversion of types *)
  let rec yojson_of_type
    ~(typevar_handling : [ `ok of Renaming.t | `disallowed_in_type_expr ])
    ~capitalization
    typ
    : Fun_or_match.t
    =
    let loc = { typ.ptyp_loc with loc_ghost = true } in
    match Ppxlib_jane.Shim.Core_type.of_parsetree typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Fun [%expr Ppx_yojson_conv_lib.Yojson_conv.yojson_of_opaque]
    | { ptyp_desc = Ptyp_any _; _ } -> Fun [%expr fun _ -> `String "_"]
    | { ptyp_desc = Ptyp_tuple labeled_tps; _ } ->
      (match Ppxlib_jane.as_unlabeled_tuple labeled_tps with
       | Some tps ->
         Match [ yojson_of_tuple ~typevar_handling ~capitalization (loc, tps) ]
       | None ->
         Location.raise_errorf ~loc "Labeled tuples unsupported in [%%yojson_of: ].")
    | { ptyp_desc = Ptyp_var (parm, _); _ } ->
      (match typevar_handling with
       | `disallowed_in_type_expr ->
         Location.raise_errorf
           ~loc
           "Type variables not allowed in [%%yojson_of: ]. Please use locally abstract \
            types instead."
       | `ok renaming ->
         (match Renaming.binding_kind renaming parm with
          | Universally_bound parm -> Fun (evar ~loc ("_of_" ^ parm))
          | Existentially_bound ->
            yojson_of_type ~typevar_handling ~capitalization [%type: _]))
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      (match typ with
       | [%type: [%t? _] yojson_opaque] ->
         Fun [%expr Ppx_yojson_conv_lib.Yojson_conv.yojson_of_opaque]
       | _ ->
         Fun
           (yojson_of_type_constr
              ~loc
              id
              (List.map args ~f:(fun tp ->
                 Fun_or_match.expr
                   ~loc
                   (yojson_of_type ~typevar_handling ~capitalization tp)))))
    | { ptyp_desc = Ptyp_arrow (_, _, _, _, _); _ } ->
      Fun
        [%expr
          fun _f ->
            Ppx_yojson_conv_lib.Yojson_conv.yojson_of_fun Ppx_yojson_conv_lib.ignore]
    | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      yojson_of_variant ~typevar_handling ~capitalization (loc, row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      yojson_of_poly ~typevar_handling ~capitalization parms poly_tp
    | { ptyp_desc; _ } ->
      Location.raise_errorf
        ~loc
        "Type unsupported for ppx [yojson_of] conversion: %s"
        (Ppxlib_jane.Language_feature_name.of_core_type_desc ptyp_desc)

  (* Conversion of tuples *)
  and yojson_of_tuple ~typevar_handling ~capitalization (loc, tps) =
    let fps =
      List.map ~f:(fun tp -> yojson_of_type ~typevar_handling ~capitalization tp) tps
    in
    let bindings, pvars, evars = Fun_or_match.map_tmp_vars ~loc fps in
    let in_expr = [%expr `List [%e elist ~loc evars]] in
    let expr = pexp_let ~loc Nonrecursive bindings in_expr in
    ppat_tuple ~loc pvars --> expr

  (* Conversion of variant types *)
  and yojson_of_variant
    ~typevar_handling
    ~capitalization
    ((loc, row_fields) : Location.t * row_field list)
    : Fun_or_match.t
    =
    let item row =
      let name_override = Attribute.get Attrs.yojson_polymorphic_variant_name row in
      match row.prf_desc with
      | Rtag (cnstr, true, []) ->
        let label =
          Label_with_name.create ~label:cnstr.txt ~name_override ~capitalization
        in
        ppat_variant ~loc (Label_with_name.label label) None
        --> [%expr `List [ `String [%e estring ~loc (Label_with_name.name label)] ]]
      | Rtag (cnstr, false, [ tp ]) ->
        let label =
          Label_with_name.create ~label:cnstr.txt ~name_override ~capitalization
        in
        let args =
          match Ppxlib_jane.Shim.Core_type_desc.of_parsetree tp.ptyp_desc with
          | Ptyp_tuple labeled_tps ->
            (match Ppxlib_jane.as_unlabeled_tuple labeled_tps with
             | Some tps -> tps
             | None -> [ tp ])
          | _ -> [ tp ]
        in
        let cnstr_expr = [%expr `String [%e estring ~loc (Label_with_name.name label)]] in
        let yojson_of_args =
          List.map ~f:(yojson_of_type ~typevar_handling ~capitalization) args
        in
        let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc yojson_of_args in
        let patt =
          match patts with
          | [ patt ] -> patt
          | _ -> ppat_tuple ~loc patts
        in
        ppat_variant (Label_with_name.label label) ~loc (Some patt)
        --> pexp_let
              ~loc
              Nonrecursive
              bindings
              [%expr `List [%e elist ~loc (cnstr_expr :: vars)]]
      | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
        ppat_alias ~loc (ppat_type ~loc id) (Loc.make "v" ~loc)
        --> yojson_of_type_constr ~loc id [ [%expr v] ]
      | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
        Location.raise_errorf ~loc "unsupported: yojson_of_variant/Rtag/&"
      | Rinherit ({ ptyp_desc = Ptyp_constr (id, _ :: _); _ } as typ) ->
        let call =
          Fun_or_match.expr ~loc (yojson_of_type ~typevar_handling ~capitalization typ)
        in
        ppat_alias ~loc (ppat_type ~loc id) (Loc.make "v" ~loc) --> [%expr [%e call] v]
      | Rinherit _ ->
        Location.raise_errorf ~loc "unsupported: yojson_of_variant/Rinherit/non-id"
      (* impossible?*)
      | Rtag (_, false, []) -> assert false
    in
    Match (List.map ~f:item row_fields)

  (* Polymorphic record fields *)
  and yojson_of_poly ~typevar_handling ~capitalization parms tp =
    let loc = tp.ptyp_loc in
    match typevar_handling with
    | `disallowed_in_type_expr ->
      (* Should be impossible because [yojson_of_poly] is only called on polymorphic record
         fields and record type definitions can't occur in type expressions. *)
      Location.raise_errorf ~loc "polymorphic type in a type expression"
    | `ok renaming ->
      let bindings =
        let mk_binding (parm, _) =
          value_binding
            ~loc
            ~pat:(pvar ~loc ("_of_" ^ parm.txt))
            ~expr:[%expr Ppx_yojson_conv_lib.Yojson_conv.yojson_of_opaque]
        in
        List.map ~f:mk_binding parms
      in
      let renaming =
        List.fold_left parms ~init:renaming ~f:Renaming.add_universally_bound
      in
      (match yojson_of_type ~typevar_handling:(`ok renaming) ~capitalization tp with
       | Fun fun_expr -> Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
       | Match matchings ->
         Match
           [ [%pat? arg]
             --> pexp_let
                   ~loc
                   Nonrecursive
                   bindings
                   (pexp_match ~loc [%expr arg] matchings)
           ])
  ;;

  (* Conversion of record types *)

  let mk_rec_patt loc patt name =
    let p = Loc.make (Longident.Lident name) ~loc, pvar ~loc ("v_" ^ name) in
    patt @ [ p ]
  ;;

  type is_empty_expr =
    | Inspect_value of (location -> expression -> expression)
    | Inspect_yojson of (cnv_expr:expression -> location -> expression -> expression)

  let yojson_of_record_field
    ~renaming
    ~capitalization
    patt
    expr
    name
    tp
    ?yojson_of
    is_empty_expr
    key
    =
    let loc = { tp.ptyp_loc with loc_ghost = true } in
    let patt = mk_rec_patt loc patt name in
    let cnv_expr =
      match yojson_of_type ~typevar_handling:(`ok renaming) ~capitalization tp with
      | Fun exp -> exp
      | Match matchings -> [%expr fun el -> [%e pexp_match ~loc [%expr el] matchings]]
    in
    let cnv_expr =
      match yojson_of with
      | None -> cnv_expr
      | Some yojson_of -> [%expr [%e yojson_of] [%e cnv_expr]]
    in
    let expr =
      let v_name = "v_" ^ name in
      [%expr
        let bnds =
          [%e
            match is_empty_expr with
            | Inspect_value is_empty_expr ->
              [%expr
                if [%e is_empty_expr loc (evar ~loc v_name)]
                then bnds
                else (
                  let arg = [%e cnv_expr] [%e evar ~loc v_name] in
                  let bnd = [%e estring ~loc key], arg in
                  bnd :: bnds)]
            | Inspect_yojson is_empty_expr ->
              [%expr
                let arg = [%e cnv_expr] [%e evar ~loc v_name] in
                if [%e is_empty_expr ~cnv_expr loc [%expr arg]]
                then bnds
                else (
                  let bnd = [%e estring ~loc key], arg in
                  bnd :: bnds)]]
        in
        [%e expr]]
    in
    patt, expr
  ;;

  let disallow_type_variables_and_recursive_occurrences ~types_being_defined ~loc ~why tp =
    let disallow_variables =
      let iter =
        object
          inherit Ast_traverse.iter as super

          method! core_type_desc t =
            match Ppxlib_jane.Shim.Core_type_desc.of_parsetree t with
            | Ptyp_var (v, _) ->
              Location.raise_errorf
                ~loc
                "[@yojson_drop_default.%s] was used, but the type of the field contains \
                 a type variable: '%s.\n\
                 Comparison is not avaiable for type variables.\n\
                 Consider using [@yojson_drop_if _] or [@yojson_drop_default.yojson] \
                 instead."
                (match why with
                 | `compare -> "compare"
                 | `equal -> "equal")
                v
            | _ -> super#core_type_desc t
        end
      in
      iter#core_type
    in
    let disallow_recursive_occurrences =
      match types_being_defined with
      | `Nonrecursive -> fun _ -> ()
      | `Recursive types_being_defined ->
        let iter =
          object
            inherit Ast_traverse.iter as super

            method! core_type_desc =
              function
              | Ptyp_constr ({ loc = _; txt = Lident s }, _) as t ->
                if Set.mem types_being_defined s
                then
                  Location.raise_errorf
                    ~loc
                    "[@yojson_drop_default.%s] was used, but the type of the field \
                     contains a type defined in the current recursive block: %s.\n\
                     This is not supported.\n\
                     Consider using [@yojson_drop_if _] or [@yojson_drop_default.yojson] \
                     instead."
                    (match why with
                     | `compare -> "compare"
                     | `equal -> "equal")
                    s;
                super#core_type_desc t
              | t -> super#core_type_desc t
          end
        in
        iter#core_type
    in
    disallow_variables tp;
    disallow_recursive_occurrences tp
  ;;

  let yojson_of_default_field
    ~types_being_defined
    how
    ~renaming
    ~capitalization
    patt
    expr
    name
    tp
    ?yojson_of
    default
    key
    =
    let is_empty =
      match how with
      | `yojson ->
        Inspect_yojson
          (fun ~cnv_expr loc yojson_expr ->
            [%expr
              Ppx_yojson_conv_lib.poly_equal ([%e cnv_expr] [%e default]) [%e yojson_expr]])
      | (`no_arg | `func _ | `compare | `equal) as how ->
        let equality_f loc =
          match how with
          | `no_arg ->
            [%expr
              Ppx_yojson_conv_lib.poly_equal
              [@ocaml.ppwarning
                "[@yojson_drop_default] is deprecated: please use one of:\n\
                 - [@yojson_drop_default f] and give an explicit equality function ([f = \
                 Poly.(=)] corresponds to the old behavior)\n\
                 - [@yojson_drop_default.compare] if the type supports [%compare]\n\
                 - [@yojson_drop_default.equal] if the type supports [%equal]\n\
                 - [@yojson_drop_default.yojson] if you want to compare the yojson \
                 representations\n"]]
          | `func f -> f
          | `compare ->
            disallow_type_variables_and_recursive_occurrences
              ~types_being_defined
              ~why:`compare
              ~loc
              tp;
            [%expr [%compare.equal: [%t tp]]]
          | `equal ->
            disallow_type_variables_and_recursive_occurrences
              ~types_being_defined
              ~why:`equal
              ~loc
              tp;
            [%expr [%equal: [%t tp]]]
        in
        Inspect_value (fun loc expr -> [%expr [%e equality_f loc] [%e default] [%e expr]])
    in
    yojson_of_record_field
      ~renaming
      ~capitalization
      patt
      expr
      name
      tp
      ?yojson_of
      is_empty
      key
  ;;

  let yojson_of_label_declaration_list
    ~types_being_defined
    ~renaming
    ~capitalization
    loc
    flds
    ~wrap_expr
    =
    let coll ((patt : (Longident.t loc * pattern) list), expr) ld =
      let name = ld.pld_name.txt in
      let key = get_label_name ld ~capitalization in
      let loc = { ld.pld_name.loc with loc_ghost = true } in
      match Attrs.Record_field_handler.Yojson_of.create ~loc ld with
      | `yojson_option tp ->
        let patt = mk_rec_patt loc patt name in
        let vname = [%expr v] in
        let cnv_expr =
          Fun_or_match.unroll
            ~loc
            vname
            (yojson_of_type ~typevar_handling:(`ok renaming) ~capitalization tp)
        in
        let expr =
          [%expr
            let bnds =
              match [%e evar ~loc ("v_" ^ name)] with
              | Ppx_yojson_conv_lib.Option.None -> bnds
              | Ppx_yojson_conv_lib.Option.Some v ->
                let arg = [%e cnv_expr] in
                let bnd = [%e estring ~loc key], arg in
                bnd :: bnds
            in
            [%e expr]]
        in
        patt, expr
      | `drop_default how ->
        let tp = ld.pld_type in
        (match Attribute.get Attrs.default ld with
         | None -> Location.raise_errorf ~loc "no default to drop"
         | Some default ->
           yojson_of_default_field
             ~types_being_defined
             how
             ~renaming
             ~capitalization
             patt
             expr
             name
             tp
             default
             key)
      | `drop_if test ->
        let tp = ld.pld_type in
        yojson_of_record_field
          ~renaming
          ~capitalization
          patt
          expr
          name
          tp
          (Inspect_value (fun loc expr -> [%expr [%e test] [%e expr]]))
          key
      | `keep as test ->
        let tp = ld.pld_type in
        let patt = mk_rec_patt loc patt name in
        let vname = evar ~loc ("v_" ^ name) in
        let cnv_expr =
          Fun_or_match.unroll
            ~loc
            vname
            (yojson_of_type ~typevar_handling:(`ok renaming) ~capitalization tp)
        in
        let bnds =
          match test with
          | `keep ->
            [%expr
              let arg = [%e cnv_expr] in
              ([%e estring ~loc key], arg) :: bnds]
          | `omit_nil ->
            [%expr
              match [%e cnv_expr] with
              | `Null -> bnds
              | arg -> ([%e estring ~loc key], arg) :: bnds]
        in
        ( patt
        , [%expr
            let bnds = [%e bnds] in
            [%e expr]] )
    in
    let init_expr = wrap_expr [%expr bnds] in
    let patt, expr = List.fold_left ~f:coll ~init:([], init_expr) flds in
    ( ppat_record ~loc patt Closed
    , [%expr
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        [%e expr]] )
  ;;

  (* Conversion of sum types *)

  let branch_sum
    ~types_being_defined
    renaming
    ~capitalization
    ~loc
    constr_lid
    constr_str
    args
    =
    match args with
    | Pcstr_record lds ->
      let cnstr_expr = [%expr `String [%e constr_str]] in
      let patt, expr =
        yojson_of_label_declaration_list
          ~types_being_defined
          ~renaming
          ~capitalization
          loc
          lds
          ~wrap_expr:(fun expr -> [%expr `List [ [%e cnstr_expr]; `Assoc [%e expr] ]])
      in
      ppat_construct ~loc constr_lid (Some patt) --> expr
    | Pcstr_tuple pcd_args ->
      (match pcd_args with
       | [] ->
         ppat_construct ~loc constr_lid None --> [%expr `List [ `String [%e constr_str] ]]
       | args ->
         let yojson_of_args =
           List.map
             ~f:(fun arg ->
               Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg
               |> yojson_of_type ~typevar_handling:(`ok renaming) ~capitalization)
             args
         in
         let cnstr_expr = [%expr `String [%e constr_str]] in
         let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc yojson_of_args in
         let patt =
           match patts with
           | [ patt ] -> patt
           | _ -> ppat_tuple ~loc patts
         in
         ppat_construct ~loc constr_lid (Some patt)
         --> pexp_let
               ~loc
               Nonrecursive
               bindings
               [%expr `List [%e elist ~loc (cnstr_expr :: vars)]])
  ;;

  let yojson_of_sum ~types_being_defined ~capitalization tps cds =
    Fun_or_match.Match
      (List.map cds ~f:(fun cd ->
         let renaming = Renaming.of_gadt tps cd in
         let constr_lid = Located.map lident cd.pcd_name in
         let constr_name =
           let label = Label_with_name.of_constructor_declaration cd ~capitalization in
           Label_with_name.name label |> estring ~loc:cd.pcd_name.loc
         in
         branch_sum
           ~types_being_defined
           renaming
           ~capitalization
           ~loc:cd.pcd_loc
           constr_lid
           constr_name
           cd.pcd_args))
  ;;

  (* Empty type *)
  let yojson_of_nil loc = Fun_or_match.Fun [%expr fun _v -> assert false]

  (* Generate code from type definitions *)

  let yojson_of_td ~types_being_defined ~capitalization td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:get_type_param_name in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let body =
      let body =
        match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
        | Ptype_variant cds ->
          yojson_of_sum
            ~types_being_defined
            ~capitalization
            (List.map tps ~f:(fun x -> x.txt))
            cds
        | Ptype_record lds ->
          let renaming = Renaming.identity in
          let patt, expr =
            yojson_of_label_declaration_list
              ~renaming
              ~capitalization
              loc
              lds
              ~types_being_defined
              ~wrap_expr:(fun expr -> [%expr `Assoc [%e expr]])
          in
          Match [ patt --> expr ]
        | Ptype_record_unboxed_product _ ->
          Location.raise_errorf ~loc "ppx_yojson_conv: unboxed record types not supported"
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_yojson_conv: open types not supported"
        | Ptype_abstract ->
          (match td.ptype_manifest with
           | None -> yojson_of_nil loc
           | Some ty ->
             yojson_of_type ~typevar_handling:(`ok Renaming.identity) ~capitalization ty)
      in
      let is_private_alias =
        match td.ptype_kind, td.ptype_manifest, td.ptype_private with
        | Ptype_abstract, Some _, Private -> true
        | _ -> false
      in
      if is_private_alias
      then (
        (* Replace all type variable by _ to avoid generalization problems *)
        let ty_src =
          core_type_of_type_declaration td |> replace_variables_by_underscores
        in
        let manifest =
          match td.ptype_manifest with
          | Some manifest -> manifest
          | None -> Location.raise_errorf ~loc "yojson_of_td/no-manifest"
        in
        let ty_dst = replace_variables_by_underscores manifest in
        let coercion = [%expr (v : [%t ty_src] :> [%t ty_dst])] in
        match body with
        | Fun fun_expr -> [%expr fun v -> [%e eapply ~loc fun_expr [ coercion ]]]
        | Match matchings -> [%expr fun v -> [%e pexp_match ~loc coercion matchings]])
      else (
        match body with
        (* Prevent violation of value restriction and problems with recursive types by
           eta-expanding function definitions *)
        | Fun fun_expr -> [%expr fun v -> [%e eapply ~loc fun_expr [ [%expr v] ]]]
        | Match matchings -> pexp_function ~loc matchings)
    in
    let typ = Sig_generate_yojson_of.mk_type td in
    let func_name = "yojson_of_" ^ type_name in
    let body =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_of_" ^ id.txt)) in
      let rec_flag =
        match types_being_defined with
        | `Recursive _ -> Recursive
        | `Nonrecursive -> Nonrecursive
      in
      eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc patts body)
    in
    [ constrained_function_binding loc td typ ~tps ~func_name body ]
  ;;

  let yojson_of_tds ~loc ~path:_ (rec_flag, tds) capitalization =
    let rec_flag = really_recursive rec_flag tds in
    let types_being_defined =
      match rec_flag with
      | Nonrecursive -> `Nonrecursive
      | Recursive ->
        `Recursive
          (Set.of_list (module String) (List.map tds ~f:(fun td -> td.ptype_name.txt)))
    in
    let bindings =
      List.concat_map tds ~f:(yojson_of_td ~types_being_defined ~capitalization)
    in
    pstr_value_list ~loc rec_flag bindings
  ;;
end

module Str_generate_yojson_fields = struct
  let yojson_fields_of_label_declaration_list ~capitalization loc flds =
    let coll ld =
      let key = get_label_name ld ~capitalization in
      let loc = ld.pld_name.loc in
      estring ~loc key
    in
    elist ~loc (List.map ~f:coll flds)
  ;;

  let yojson_fields_of_td ~capitalization td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:get_type_param_name in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let body =
      match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
      | Ptype_record lds ->
        yojson_fields_of_label_declaration_list ~capitalization loc lds
      | Ptype_record_unboxed_product _ ->
        Location.raise_errorf
          ~loc
          "ppx_yojson_conv: yojson_fields does not yet support unboxed records"
      | Ptype_variant _ | Ptype_open | Ptype_abstract ->
        Location.raise_errorf ~loc "ppx_yojson_conv: yojson_fields only works on records"
    in
    let typ = [%type: string list] in
    let func_name = "yojson_fields_of_" ^ type_name in
    let body =
      let patts = List.map tps ~f:(fun id -> pvar ~loc ("_fields_of_" ^ id.txt)) in
      eta_reduce_if_possible (eabstract ~loc patts body)
    in
    constrained_function_binding loc td typ ~tps ~func_name body
  ;;

  let yojson_fields_of_tds ~loc ~path:_ (_, tds) capitalization =
    pstr_value_list
      ~loc
      Nonrecursive
      (List.map tds ~f:(yojson_fields_of_td ~capitalization))
  ;;
end

module Str_generate_of_yojson = struct
  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    [ [%pat? Ppx_yojson_conv_lib.Yojson_conv_error.No_variant_match] --> expr ]
  ;;

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc ~match_last matches =
    match match_last, matches with
    | true, [ { pc_lhs = _; pc_guard = None; pc_rhs = expr } ]
    | _, [ { pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = expr } ] -> expr
    | _ -> pexp_match ~loc [%expr atom] matches
  ;;

  (* Generate code for matching malformed Yojsons *)
  let mk_variant_other_matches loc ~rev_els call =
    let coll_structs acc (loc, label) =
      (pstring ~loc (Label_with_name.name label)
       -->
       match call with
       | `ptag_no_args ->
         [%expr Ppx_yojson_conv_lib.Yojson_conv_error.ptag_no_args _tp_loc _yojson]
       | `ptag_takes_args ->
         [%expr Ppx_yojson_conv_lib.Yojson_conv_error.ptag_takes_args _tp_loc _yojson])
      :: acc
    in
    let exc_no_variant_match =
      [%pat? _] --> [%expr Ppx_yojson_conv_lib.Yojson_conv_error.no_variant_match ()]
    in
    List.fold_left ~f:coll_structs ~init:[ exc_no_variant_match ] rev_els
  ;;

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let split_row_field ~loc ~capitalization (atoms, structs, ainhs, sinhs) row_field =
    let name_override = Attribute.get Attrs.yojson_polymorphic_variant_name row_field in
    match row_field.prf_desc with
    | Rtag (cnstr, true, []) ->
      let label =
        Label_with_name.create ~label:cnstr.txt ~name_override ~capitalization
      in
      let tpl = loc, label in
      tpl :: atoms, structs, `A tpl :: ainhs, sinhs
    | Rtag (cnstr, false, [ tp ]) ->
      let label =
        Label_with_name.create ~label:cnstr.txt ~name_override ~capitalization
      in
      let loc = tp.ptyp_loc in
      atoms, (loc, label) :: structs, ainhs, `S (loc, label, tp, row_field) :: sinhs
    | Rinherit inh ->
      let iinh = `I inh in
      atoms, structs, iinh :: ainhs, iinh :: sinhs
    | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
      Location.raise_errorf ~loc "split_row_field/&"
    | Rtag (_, false, []) -> assert false
  ;;

  let type_constr_of_yojson ?(internal = false) ~loc id args =
    type_constr_conv id args ~loc ~f:(fun s ->
      let s = s ^ "_of_yojson" in
      if internal then "__" ^ s ^ "__" else s)
  ;;

  (* Conversion of types *)
  let rec type_of_yojson
    ~typevar_handling
    ~capitalization
    ?full_type
    ?(internal = false)
    typ
    : Fun_or_match.t
    =
    let loc = { typ.ptyp_loc with loc_ghost = true } in
    match typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Fun [%expr Ppx_yojson_conv_lib.Yojson_conv.opaque_of_yojson]
    | [%type: [%t? _] yojson_opaque] | [%type: _] ->
      Fun [%expr Ppx_yojson_conv_lib.Yojson_conv.opaque_of_yojson]
    | _ ->
      (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ.ptyp_desc with
       | Ptyp_tuple labeled_tps ->
         (match Ppxlib_jane.as_unlabeled_tuple labeled_tps with
          | Some tps ->
            Match (tuple_of_yojson ~typevar_handling ~capitalization (loc, tps))
          | None ->
            Location.raise_errorf ~loc "Labeled tuples unsupported in [%%of_yojson: ].")
       | Ptyp_var (parm, _) ->
         (match typevar_handling with
          | `ok -> Fun (evar ~loc ("_of_" ^ parm))
          | `disallowed_in_type_expr ->
            Location.raise_errorf
              ~loc
              "Type variables not allowed in [%%of_yojson: ]. Please use locally \
               abstract types instead.")
       | Ptyp_constr (id, args) ->
         let args =
           List.map args ~f:(fun arg ->
             Fun_or_match.expr ~loc (type_of_yojson ~typevar_handling ~capitalization arg))
         in
         Fun (type_constr_of_yojson ~loc ~internal id args)
       | Ptyp_arrow (_, _, _, _, _) ->
         Fun [%expr Ppx_yojson_conv_lib.Yojson_conv.fun_of_yojson]
       | Ptyp_variant (row_fields, _, _) ->
         variant_of_yojson ~typevar_handling ~capitalization ?full_type (loc, row_fields)
       | Ptyp_poly (parms, poly_tp) ->
         poly_of_yojson ~typevar_handling ~capitalization parms poly_tp
       | Ptyp_any _ ->
         (* This case is matched in the outer match *)
         failwith "impossible state"
       | typ ->
         Location.raise_errorf
           ~loc
           "Type unsupported for ppx [of_yojson] conversion: %s"
           (Ppxlib_jane.Language_feature_name.of_core_type_desc typ))

  (* Conversion of tuples *)
  and tuple_of_yojson ~typevar_handling ~capitalization (loc, tps) =
    let fps = List.map ~f:(type_of_yojson ~typevar_handling ~capitalization) tps in
    let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
    let n = List.length fps in
    [ [%pat? `List [%p plist ~loc patts]]
      --> pexp_let ~loc Nonrecursive bindings (pexp_tuple ~loc vars)
    ; [%pat? yojson]
      --> [%expr
            Ppx_yojson_conv_lib.Yojson_conv_error.tuple_of_size_n_expected
              _tp_loc
              [%e eint ~loc n]
              yojson]
    ]

  (* Generate code for matching included variant types *)
  and handle_variant_inh
    ~typevar_handling
    ~capitalization
    full_type
    ~match_last
    other_matches
    inh
    =
    let loc = inh.ptyp_loc in
    let func_expr = type_of_yojson ~typevar_handling ~capitalization ~internal:true inh in
    let app : Fun_or_match.t =
      let fun_expr = Fun_or_match.expr ~loc func_expr in
      Fun [%expr [%e fun_expr] _yojson]
    in
    let match_exc =
      handle_no_variant_match
        loc
        (handle_variant_match_last loc ~match_last other_matches)
    in
    let new_other_matches =
      [ [%pat? _]
        --> pexp_try
              ~loc
              [%expr
                ([%e Fun_or_match.expr ~loc app]
                  :> [%t replace_variables_by_underscores full_type])]
              match_exc
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom
    ~typevar_handling
    ~capitalization
    loc
    full_type
    ~rev_atoms_inhs
    ~rev_structs
    =
    let coll (other_matches, match_last) = function
      | `A (loc, label) ->
        let new_match =
          pstring ~loc (Label_with_name.name label)
          --> pexp_variant ~loc (Label_with_name.label label) None
        in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh
          ~typevar_handling
          ~capitalization
          full_type
          ~match_last
          other_matches
          inh
    in
    let other_matches =
      mk_variant_other_matches loc ~rev_els:rev_structs `ptag_takes_args
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs
    in
    handle_variant_match_last loc ~match_last match_atoms_inhs

  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match ~typevar_handling ~capitalization ~loc ~is_variant label tps =
    let cnstr_label = Label_with_name.label label in
    let cnstr vars_expr =
      if is_variant
      then pexp_variant ~loc cnstr_label (Some vars_expr)
      else pexp_construct ~loc (Located.lident ~loc cnstr_label) (Some vars_expr)
    in
    let bindings, patts, good_arg_match =
      let fps = List.map ~f:(type_of_yojson ~typevar_handling ~capitalization) tps in
      let bindings, patts, vars = Fun_or_match.map_tmp_vars ~loc fps in
      let good_arg_match =
        let vars_expr =
          match vars with
          | [ var_expr ] -> var_expr
          | _ -> pexp_tuple ~loc vars
        in
        cnstr vars_expr
      in
      bindings, patts, good_arg_match
    in
    [%expr
      match yojson_args with
      | [%p plist ~loc patts] -> [%e pexp_let ~loc Nonrecursive bindings good_arg_match]
      | _ ->
        [%e
          if is_variant
          then
            [%expr
              Ppx_yojson_conv_lib.Yojson_conv_error.ptag_incorrect_n_args
                _tp_loc
                _tag
                _yojson]
          else
            [%expr
              Ppx_yojson_conv_lib.Yojson_conv_error.stag_incorrect_n_args
                _tp_loc
                _tag
                _yojson]]]

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct
    ~typevar_handling
    ~capitalization
    loc
    full_type
    ~rev_structs_inhs
    ~rev_atoms
    =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, label, tp, _row) ->
        has_structs_ref := true;
        let args =
          match Ppxlib_jane.Shim.Core_type_desc.of_parsetree tp.ptyp_desc with
          | Ptyp_tuple labeled_tps ->
            (match Ppxlib_jane.as_unlabeled_tuple labeled_tps with
             | Some tps -> tps
             | None -> [ tp ])
          | _ -> [ tp ]
        in
        let expr =
          mk_cnstr_args_match
            ~typevar_handling
            ~capitalization
            ~loc:tp.ptyp_loc
            ~is_variant:true
            label
            args
        in
        let new_match =
          [%pat? [%p pstring ~loc (Label_with_name.name label)] as _tag] --> expr
        in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh
          ~typevar_handling
          ~capitalization
          full_type
          ~match_last
          other_matches
          inh
    in
    let other_matches = mk_variant_other_matches loc ~rev_els:rev_atoms `ptag_no_args in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    handle_variant_match_last loc ~match_last match_structs_inhs, !has_structs_ref

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag ~typevar_handling ~capitalization loc full_type row_field_list =
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left
        ~f:(split_row_field ~loc ~capitalization)
        ~init:([], [], [], [])
        row_field_list
    in
    let match_struct, has_structs =
      mk_variant_match_struct
        ~typevar_handling
        ~capitalization
        loc
        full_type
        ~rev_structs_inhs
        ~rev_atoms
    in
    let maybe_yojson_args_patt = if has_structs then [%pat? yojson_args] else [%pat? _] in
    [ [%pat? `List [ `String atom ] as _yojson]
      --> mk_variant_match_atom
            ~typevar_handling
            ~capitalization
            loc
            full_type
            ~rev_atoms_inhs
            ~rev_structs
    ; [%pat? `List (`String atom :: [%p maybe_yojson_args_patt]) as _yojson]
      --> match_struct
    ; [%pat? `List (`List _ :: _) as yojson]
      --> [%expr
            Ppx_yojson_conv_lib.Yojson_conv_error.nested_list_invalid_poly_var
              _tp_loc
              yojson]
    ; [%pat? `List [] as yojson]
      --> [%expr
            Ppx_yojson_conv_lib.Yojson_conv_error.empty_list_invalid_poly_var
              _tp_loc
              yojson]
    ; [%pat? _ as yojson]
      --> [%expr Ppx_yojson_conv_lib.Yojson_conv_error.unexpected_stag _tp_loc yojson]
    ]

  (* Generate matching code for variants *)
  and variant_of_yojson ~typevar_handling ~capitalization ?full_type (loc, row_fields) =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let top_match =
      match row_fields with
      | { prf_desc = Rinherit inh; _ } :: rest ->
        let rec loop inh row_fields =
          let call =
            [%expr
              ([%e
                 Fun_or_match.expr
                   ~loc
                   (type_of_yojson ~typevar_handling ~capitalization ~internal:true inh)]
                 yojson
                :> [%t replace_variables_by_underscores full_type])]
          in
          match row_fields with
          | [] -> call
          | h :: t ->
            let expr =
              match h.prf_desc with
              | Rinherit inh -> loop inh t
              | _ ->
                let rftag_matches =
                  handle_variant_tag
                    ~typevar_handling
                    ~capitalization
                    loc
                    full_type
                    row_fields
                in
                pexp_match ~loc [%expr yojson] rftag_matches
            in
            pexp_try ~loc call (handle_no_variant_match loc expr)
        in
        [ [%pat? yojson] --> loop inh rest ]
      | _ :: _ ->
        handle_variant_tag ~typevar_handling ~capitalization loc full_type row_fields
      | [] -> assert false
      (* impossible *)
    in
    if is_contained
    then
      Fun
        [%expr
          fun yojson ->
            try [%e pexp_match ~loc [%expr yojson] top_match] with
            | Ppx_yojson_conv_lib.Yojson_conv_error.No_variant_match ->
              Ppx_yojson_conv_lib.Yojson_conv_error.no_matching_variant_found
                _tp_loc
                yojson]
    else Match top_match

  and poly_of_yojson ~typevar_handling ~capitalization parms tp =
    let loc = tp.ptyp_loc in
    let bindings =
      let mk_binding (parm, _) =
        value_binding
          ~loc
          ~pat:(pvar ~loc ("_of_" ^ parm.txt))
          ~expr:
            [%expr
              fun yojson ->
                Ppx_yojson_conv_lib.Yojson_conv_error.record_poly_field_value
                  _tp_loc
                  yojson]
      in
      List.map ~f:mk_binding parms
    in
    match type_of_yojson ~typevar_handling ~capitalization tp with
    | Fun fun_expr -> Fun (pexp_let ~loc Nonrecursive bindings fun_expr)
    | Match matchings ->
      Match
        [ [%pat? arg]
          --> pexp_let ~loc Nonrecursive bindings (pexp_match ~loc [%expr arg] matchings)
        ]
  ;;

  (* Generate code for extracting record fields *)
  let mk_extract_fields ~typevar_handling ~capitalization ~allow_extra_fields (loc, flds) =
    let rec loop inits no_args args = function
      | [] -> inits, no_args, args
      | ld :: more_flds ->
        let loc = ld.pld_name.loc in
        let nm = ld.pld_name.txt in
        let key = get_label_name ld ~capitalization in
        (match Attrs.Record_field_handler.Of_yojson.create ~loc ld, ld.pld_type with
         | Some (`yojson_option tp), _ | (None | Some (`default _)), tp ->
           let inits = [%expr Ppx_yojson_conv_lib.Option.None] :: inits in
           let unrolled =
             Fun_or_match.unroll
               ~loc
               [%expr _field_yojson]
               (type_of_yojson ~typevar_handling ~capitalization tp)
           in
           let args =
             (pstring ~loc key
              --> [%expr
                    match Ppx_yojson_conv_lib.( ! ) [%e evar ~loc (nm ^ "_field")] with
                    | Ppx_yojson_conv_lib.Option.None ->
                      let fvalue = [%e unrolled] in
                      [%e evar ~loc (nm ^ "_field")]
                      := Ppx_yojson_conv_lib.Option.Some fvalue
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                      duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates])
             :: args
           in
           loop inits no_args args more_flds)
    in
    let handle_extra =
      [ ([%pat? _]
         -->
         if allow_extra_fields
         then [%expr ()]
         else
           [%expr
             if Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
             then extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
             else ()])
      ]
    in
    loop [] handle_extra handle_extra (List.rev flds)
  ;;

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result has_poly (loc, flds) ~wrap_expr =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop ((res_tpls, bi_lst, good_patts) as acc) = function
        | ({ pld_name = { txt = nm; loc }; _ } as ld) :: more_flds ->
          let fld = [%expr Ppx_yojson_conv_lib.( ! ) [%e evar ~loc (nm ^ "_field")]] in
          let mk_default loc =
            bi_lst, [%pat? [%p pvar ~loc (nm ^ "_value")]] :: good_patts
          in
          let new_bi_lst, new_good_patts =
            match Attrs.Record_field_handler.Of_yojson.create ~loc ld with
            | Some (`default _ | `yojson_option _) -> mk_default loc
            | None ->
              has_nonopt_fields := true;
              ( [%expr
                  Ppx_yojson_conv_lib.poly_equal [%e fld] Ppx_yojson_conv_lib.Option.None
                  , [%e estring ~loc nm]]
                :: bi_lst
              , [%pat? Ppx_yojson_conv_lib.Option.Some [%p pvar ~loc (nm ^ "_value")]]
                :: good_patts )
          in
          let acc = [%expr [%e fld]] :: res_tpls, new_bi_lst, new_good_patts in
          loop acc more_flds
        | [] -> acc
      in
      loop ([], [], []) (List.rev flds)
    in
    let match_good_expr =
      if has_poly
      then (
        let cnvt = function
          | { pld_name = { txt = nm; _ }; _ } -> evar ~loc (nm ^ "_value")
        in
        match List.map ~f:cnvt flds with
        | [ match_good_expr ] -> match_good_expr
        | match_good_exprs -> pexp_tuple ~loc match_good_exprs)
      else (
        let cnvt ld =
          let nm = ld.pld_name.txt in
          let value =
            match Attrs.Record_field_handler.Of_yojson.create ~loc ld with
            | Some (`default default) ->
              [%expr
                match [%e evar ~loc (nm ^ "_value")] with
                | Ppx_yojson_conv_lib.Option.None -> [%e default]
                | Ppx_yojson_conv_lib.Option.Some v -> v]
            | Some (`yojson_option _) | None -> evar ~loc (nm ^ "_value")
          in
          Located.lident ~loc nm, value
        in
        wrap_expr (pexp_record ~loc (List.map ~f:cnvt flds) None))
    in
    let expr, patt =
      match res_tpls, good_patts with
      | [ res_expr ], [ res_patt ] -> res_expr, res_patt
      | _ -> pexp_tuple ~loc res_tpls, ppat_tuple ~loc good_patts
    in
    if !has_nonopt_fields
    then
      pexp_match
        ~loc
        expr
        [ patt --> match_good_expr
        ; [%pat? _]
          --> [%expr
                Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                  _tp_loc
                  yojson
                  [%e elist ~loc bi_lst]]
        ]
    else pexp_match ~loc expr [ patt --> match_good_expr ]
  ;;

  (* Generate code for converting record fields *)

  let mk_cnv_fields
    ~typevar_handling
    ~capitalization
    ~allow_extra_fields
    has_poly
    (loc, flds)
    ~wrap_expr
    =
    let expr_ref_inits, _mc_no_args_fields, mc_fields_with_args =
      mk_extract_fields ~typevar_handling ~capitalization ~allow_extra_fields (loc, flds)
    in
    let field_refs =
      List.map2_exn
        flds
        expr_ref_inits
        ~f:(fun { pld_name = { txt = name; loc }; _ } init ->
          value_binding
            ~loc
            ~pat:(pvar ~loc (name ^ "_field"))
            ~expr:[%expr ref [%e init]])
    in
    pexp_let
      ~loc
      Nonrecursive
      (field_refs
       @ [ value_binding ~loc ~pat:[%pat? duplicates] ~expr:[%expr ref []]
         ; value_binding ~loc ~pat:[%pat? extra] ~expr:[%expr ref []]
         ])
      [%expr
        let rec iter =
          [%e
            pexp_function
              ~loc
              [ [%pat? (field_name, _field_yojson) :: tail]
                --> [%expr
                      [%e pexp_match ~loc [%expr field_name] mc_fields_with_args];
                      iter tail]
              ; [%pat? []] --> [%expr ()]
              ]]
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
            _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] ->
          (match Ppx_yojson_conv_lib.( ! ) extra with
           | _ :: _ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
               _tp_loc
               (Ppx_yojson_conv_lib.( ! ) extra)
               yojson
           | [] -> [%e mk_handle_record_match_result has_poly (loc, flds) ~wrap_expr])]
  ;;

  let is_poly (_, flds) =
    List.exists flds ~f:(function
      | { pld_type = { ptyp_desc = Ptyp_poly _; _ }; _ } -> true
      | _ -> false)
  ;;

  let label_declaration_list_of_yojson
    ~typevar_handling
    ~capitalization
    ~allow_extra_fields
    loc
    flds
    ~wrap_expr
    =
    let has_poly = is_poly (loc, flds) in
    let cnv_fields =
      mk_cnv_fields
        ~typevar_handling
        ~capitalization
        ~allow_extra_fields
        has_poly
        (loc, flds)
        ~wrap_expr
    in
    if has_poly
    then (
      let patt =
        let pats =
          List.map flds ~f:(fun { pld_name = { txt = name; loc }; _ } -> pvar ~loc name)
        in
        match pats with
        | [ pat ] -> pat
        | pats -> ppat_tuple ~loc pats
      in
      let record_def =
        wrap_expr
          (pexp_record
             ~loc
             (List.map flds ~f:(fun { pld_name = { txt = name; loc }; _ } ->
                Located.lident ~loc name, evar ~loc name))
             None)
      in
      pexp_let
        ~loc
        Nonrecursive
        [ value_binding ~loc ~pat:patt ~expr:cnv_fields ]
        record_def)
    else cnv_fields
  ;;

  (* Generate matching code for records *)
  let record_of_yojson ~typevar_handling ~capitalization ~allow_extra_fields (loc, flds)
    : Fun_or_match.t
    =
    Match
      [ [%pat? `Assoc field_yojsons as yojson]
        --> label_declaration_list_of_yojson
              ~typevar_handling
              ~capitalization
              ~allow_extra_fields
              loc
              flds
              ~wrap_expr:(fun x -> x)
      ; [%pat? _ as yojson]
        --> [%expr
              Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
                _tp_loc
                yojson]
      ]
  ;;

  (* Sum type conversions *)
  (* Generate matching code for well-formed Yojsons wrt. sum types *)
  let mk_good_sum_matches ~typevar_handling ~capitalization (loc, cds) =
    List.map cds ~f:(fun (cd, label) ->
      let cnstr_label = Label_with_name.label label in
      let cnstr_name = Label_with_name.name label in
      match cd with
      | { pcd_args = Pcstr_record fields; _ } ->
        let expr =
          label_declaration_list_of_yojson
            ~typevar_handling
            ~capitalization
            ~allow_extra_fields:
              (Option.is_some (Attribute.get Attrs.allow_extra_fields_cd cd))
            loc
            fields
            ~wrap_expr:(fun e ->
              pexp_construct ~loc (Located.lident ~loc cnstr_label) (Some e))
        in
        [%pat?
          `List [ `String ([%p pstring ~loc cnstr_name] as _tag); `Assoc field_yojsons ]
          as yojson]
        --> expr
      | { pcd_args = Pcstr_tuple []; _ } ->
        Attrs.fail_if_allow_extra_field_cd ~loc cd;
        [%pat? `List [ `String [%p pstring ~loc cnstr_name] ]]
        --> pexp_construct ~loc (Located.lident ~loc cnstr_label) None
      | { pcd_args = Pcstr_tuple (_ :: _ as args); _ } ->
        let tps = List.map args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type in
        Attrs.fail_if_allow_extra_field_cd ~loc cd;
        [%pat?
          `List (`String ([%p pstring ~loc cnstr_name] as _tag) :: yojson_args) as _yojson]
        --> mk_cnstr_args_match
              ~typevar_handling
              ~capitalization
              ~loc
              ~is_variant:false
              label
              tps)
  ;;

  (* Generate matching code for malformed Yojsons with good tags
     wrt. sum types *)
  let mk_bad_sum_matches (loc, cds) =
    List.map cds ~f:(fun (cd, label) ->
      let cnstr_name = Label_with_name.name label in
      match cd with
      | { pcd_args = Pcstr_tuple []; _ } ->
        [%pat? `List (`String [%p pstring ~loc cnstr_name] :: _) as yojson]
        --> [%expr Ppx_yojson_conv_lib.Yojson_conv_error.stag_no_args _tp_loc yojson]
      | { pcd_args = Pcstr_tuple (_ :: _) | Pcstr_record _; _ } ->
        [%pat? `String [%p pstring ~loc cnstr_name] as yojson]
        --> [%expr Ppx_yojson_conv_lib.Yojson_conv_error.stag_takes_args _tp_loc yojson])
  ;;

  (* Generate matching code for sum types *)
  let sum_of_yojson ~typevar_handling ~capitalization (loc, alts) : Fun_or_match.t =
    let alts =
      List.map alts ~f:(fun cd ->
        cd, Label_with_name.of_constructor_declaration cd ~capitalization)
    in
    Match
      (List.concat
         [ mk_good_sum_matches ~typevar_handling ~capitalization (loc, alts)
         ; mk_bad_sum_matches (loc, alts)
         ; [ [%pat? `List (`List _ :: _) as yojson]
             --> [%expr
                   Ppx_yojson_conv_lib.Yojson_conv_error.nested_list_invalid_sum
                     _tp_loc
                     yojson]
           ; [%pat? `List [] as yojson]
             --> [%expr
                   Ppx_yojson_conv_lib.Yojson_conv_error.empty_list_invalid_sum
                     _tp_loc
                     yojson]
           ; [%pat? _ as yojson]
             --> [%expr
                   Ppx_yojson_conv_lib.Yojson_conv_error.unexpected_stag _tp_loc yojson]
           ]
         ])
  ;;

  (* Empty type *)
  let nil_of_yojson loc : Fun_or_match.t =
    Fun
      [%expr
        fun yojson -> Ppx_yojson_conv_lib.Yojson_conv_error.empty_type _tp_loc yojson]
  ;;

  (* Generate code from type definitions *)

  let td_of_yojson ~typevar_handling ~capitalization ~loc:_ ~poly ~path ~rec_flag td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:get_type_param_name in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let full_type =
      core_type_of_type_declaration td |> replace_variables_by_underscores
    in
    let is_private =
      match td.ptype_private with
      | Private -> true
      | Public -> false
    in
    if is_private
    then Location.raise_errorf ~loc "of_yojson is not supported for private type";
    let create_internal_function =
      match is_polymorphic_variant td ~sig_:false with
      | `Definitely -> true
      | `Maybe -> poly
      | `Surely_not ->
        if poly
        then
          Location.raise_errorf
            ~loc
            "yojson_poly annotation on a type that is surely not a polymorphic variant";
        false
    in
    let body =
      let body =
        match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
        | Ptype_variant alts ->
          Attrs.fail_if_allow_extra_field_td ~loc td;
          sum_of_yojson ~typevar_handling ~capitalization (td.ptype_loc, alts)
        | Ptype_record lbls ->
          record_of_yojson
            ~typevar_handling
            ~capitalization
            ~allow_extra_fields:
              (Option.is_some (Attribute.get Attrs.allow_extra_fields_td td))
            (loc, lbls)
        | Ptype_record_unboxed_product _ ->
          Location.raise_errorf ~loc "ppx_yojson_conv: unboxed record types not supported"
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_yojson_conv: open types not supported"
        | Ptype_abstract ->
          Attrs.fail_if_allow_extra_field_td ~loc td;
          (match td.ptype_manifest with
           | None -> nil_of_yojson td.ptype_loc
           | Some ty ->
             type_of_yojson
               ~full_type
               ~typevar_handling
               ~capitalization
               ~internal:create_internal_function
               ty)
      in
      match body with
      (* Prevent violation of value restriction and problems with
         recursive types by eta-expanding function definitions *)
      | Fun fun_expr -> [%expr fun t -> [%e eapply ~loc fun_expr [ [%expr t] ]]]
      | Match matchings -> pexp_function ~loc matchings
    in
    let external_name = type_name ^ "_of_yojson" in
    let internal_name = "__" ^ type_name ^ "_of_yojson__" in
    let arg_patts, arg_exprs =
      List.unzip
        (List.map
           ~f:(fun tp ->
             let name = "_of_" ^ tp.txt in
             pvar ~loc name, evar ~loc name)
           tps)
    in
    let bind_tp_loc_in =
      let full_type_name = Printf.sprintf "%s.%s" path type_name in
      fun e ->
        match e with
        | { pexp_desc = Pexp_ident _; _ } ->
          (* we definitely don't use the string, so clean up the generated code a bit *)
          e
        | _ ->
          [%expr
            let _tp_loc = [%e estring ~loc full_type_name] in
            [%e e]]
    in
    let internal_fun_body =
      if create_internal_function
      then
        Some
          (bind_tp_loc_in
             (eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc arg_patts body)))
      else None
    in
    let external_fun_body =
      let need_tp_loc, body_below_lambdas =
        if create_internal_function
        then (
          let no_variant_match_mc =
            [ [%pat? Ppx_yojson_conv_lib.Yojson_conv_error.No_variant_match]
              --> [%expr
                    Ppx_yojson_conv_lib.Yojson_conv_error.no_matching_variant_found
                      _tp_loc
                      yojson]
            ]
          in
          let internal_call =
            let internal_expr = evar ~loc internal_name in
            eapply ~loc internal_expr (arg_exprs @ [ [%expr yojson] ])
          in
          let try_with = pexp_try ~loc internal_call no_variant_match_mc in
          false, bind_tp_loc_in [%expr fun yojson -> [%e try_with]])
        else true, body
      in
      let body_with_lambdas =
        eta_reduce_if_possible_and_nonrec
          ~rec_flag
          (eabstract ~loc arg_patts body_below_lambdas)
      in
      if need_tp_loc then bind_tp_loc_in body_with_lambdas else body_with_lambdas
    in
    let mk_binding func_name body =
      let typ = Sig_generate_of_yojson.mk_type td in
      constrained_function_binding loc td typ ~tps ~func_name body
    in
    let internal_bindings =
      match internal_fun_body with
      | None -> []
      | Some body -> [ mk_binding internal_name body ]
    in
    let external_binding = mk_binding external_name external_fun_body in
    internal_bindings, [ external_binding ]
  ;;

  (* Generate code from type definitions *)
  let tds_of_yojson ~loc ~poly ~path (rec_flag, tds) capitalization =
    let typevar_handling = `ok in
    let singleton =
      match tds with
      | [ _ ] -> true
      | _ -> false
    in
    if singleton
    then (
      let rec_flag = really_recursive rec_flag tds in
      match rec_flag with
      | Recursive ->
        let bindings =
          List.concat_map tds ~f:(fun td ->
            let internals, externals =
              td_of_yojson ~typevar_handling ~capitalization ~loc ~poly ~path ~rec_flag td
            in
            internals @ externals)
        in
        pstr_value_list ~loc Recursive bindings
      | Nonrecursive ->
        List.concat_map tds ~f:(fun td ->
          let internals, externals =
            td_of_yojson ~typevar_handling ~capitalization ~loc ~poly ~path ~rec_flag td
          in
          pstr_value_list ~loc Nonrecursive internals
          @ pstr_value_list ~loc Nonrecursive externals))
    else (
      let bindings =
        List.concat_map tds ~f:(fun td ->
          let internals, externals =
            td_of_yojson ~typevar_handling ~capitalization ~poly ~loc ~path ~rec_flag td
          in
          internals @ externals)
      in
      pstr_value_list ~loc rec_flag bindings)
  ;;

  let type_of_yojson ~typevar_handling ~capitalization ~path ctyp =
    let loc = { ctyp.ptyp_loc with loc_ghost = true } in
    let fp = type_of_yojson ~typevar_handling ~capitalization ctyp in
    let body =
      match fp with
      | Fun fun_expr -> [%expr [%e fun_expr] yojson]
      | Match matchings -> pexp_match ~loc [%expr yojson] matchings
    in
    let full_type_name =
      Printf.sprintf
        "%s line %i: %s"
        path
        loc.loc_start.pos_lnum
        (string_of_core_type ctyp)
    in
    [%expr
      fun yojson ->
        let _tp_loc = [%e estring ~loc full_type_name] in
        [%e body]]
  ;;
end

module Yojson_of = struct
  let type_extension ty =
    Sig_generate_yojson_of.type_of_yojson_of ~loc:{ ty.ptyp_loc with loc_ghost = true } ty
  ;;

  let core_type ty =
    Str_generate_yojson_of.yojson_of_type
      ~typevar_handling:`disallowed_in_type_expr
      ~capitalization:None
      ty
    |> Fun_or_match.expr ~loc:{ ty.ptyp_loc with loc_ghost = true }
  ;;

  let sig_type_decl = Sig_generate_yojson_of.mk_sig
  let str_type_decl = Str_generate_yojson_of.yojson_of_tds
end

module Yojson_fields = struct
  let str_type_decl = Str_generate_yojson_fields.yojson_fields_of_tds
end

module Of_yojson = struct
  let type_extension ty =
    Sig_generate_of_yojson.type_of_of_yojson ~loc:{ ty.ptyp_loc with loc_ghost = true } ty
  ;;

  let core_type =
    Str_generate_of_yojson.type_of_yojson
      ~typevar_handling:`disallowed_in_type_expr
      ~capitalization:None
  ;;

  let sig_type_decl = Sig_generate_of_yojson.mk_sig
  let str_type_decl = Str_generate_of_yojson.tds_of_yojson
end

module Sig_yojson = struct
  let mk_sig ~loc ~path decls =
    Sig_generate_yojson_of.mk_sig ~loc ~path decls
    @ Sig_generate_of_yojson.mk_sig ~poly:false ~loc ~path decls
  ;;

  let sig_type_decl ~loc ~path ((_rf, tds) as decls) =
    match
      mk_named_sig
        ~loc
        ~sg_name:"Ppx_yojson_conv_lib.Yojsonable.S"
        ~handle_polymorphic_variant:false
        tds
    with
    | Some include_infos -> [ psig_include ~loc include_infos ]
    | None -> mk_sig ~loc ~path decls
  ;;
end
