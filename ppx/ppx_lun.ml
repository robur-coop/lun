open Ppxlib
module List = ListLabels
open Ast_builder.Default
module H = Ast_helper

let ( $. ) l x = Longident.Ldot (l, x)
let lun = Longident.Lident "Lun"
let unit = lident "unit"
let str fmt = Format.kasprintf Fun.id fmt

let attr_set_warning ~loc s =
  attribute ~loc ~name:(Located.mk ~loc "warning")
    ~payload:(PStr [pstr_eval ~loc (estring ~loc s) []])
let set_warning s e =
  H.Exp.attr e (attr_set_warning ~loc:e.pexp_loc s)

let random_string ~len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    let chr =
      match Random.int (10 + 26 + 26) with
      | n when n < 10 -> Char.chr (Char.code '0' + n)
      | n when n < 10 + 26 -> Char.chr (Char.code 'a' + (n - 10))
      | n -> Char.chr (Char.code 'A' + (n - 10 - 26))
    in
    Bytes.set res i chr
  done ;
  Bytes.unsafe_to_string res

let var ?(len = 6) prefix = str "%s%s" prefix (random_string ~len)

let lense_impl ~name ~uniq (ld : label_declaration) =
  let field_name = ld.pld_name.txt in
  let loc = ld.pld_loc in
  let prj =
    let var = var "v" in
    pexp_fun ~loc Nolabel None
      (ppat_var ~loc { loc; txt = var })
      (pexp_field ~loc
         (pexp_ident ~loc { loc; txt = lident var })
         { loc; txt = lident field_name })
  in
  let inj =
    let var = var "v" in
    pexp_fun ~loc Nolabel None
      (if uniq then ppat_any ~loc else ppat_var ~loc { loc; txt = var })
      (pexp_fun ~loc Nolabel None
         (ppat_var ~loc { loc; txt = field_name })
         (pexp_record ~loc
            [
              ( { loc; txt = lident field_name }
              , pexp_ident ~loc { loc; txt = lident field_name } )
            ]
            (if uniq then None
            else Some (pexp_ident ~loc { loc; txt = lident var }))))
  in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc { loc; txt = str "%s_%s" name field_name }
      ; pvb_expr =
          pexp_fun ~loc Nolabel None (punit ~loc)
            (pexp_apply ~loc
               (pexp_ident ~loc { loc; txt = lun $. "lense" })
               [ (Nolabel, prj); (Nolabel, inj) ])
      ; pvb_attributes = []
      ; pvb_loc = loc
      ; pvb_constraint = None
      }
    ]

let error_case ~loc =
  case
    ~lhs:(ppat_var ~loc { loc; txt = "v" })
    ~guard:None
    ~rhs:
      (pexp_apply ~loc
         (pexp_ident ~loc { loc; txt = lident "Result" $. "error" })
         [ (Nolabel, evar ~loc "v") ])

let prism_impl ~name ~uniq (ctor : constructor_declaration) =
  let ctor_name = ctor.pcd_name.txt in
  let loc = ctor.pcd_name.loc in
  let inj, prj =
    match ctor.pcd_args with
    | Pcstr_tuple [] ->
        let inj =
          pexp_fun ~loc Nolabel None (punit ~loc) (econstruct ctor None)
        in
        let prj =
          let lhs = pconstruct ctor None in
          let rhs =
            pexp_apply ~loc
              (pexp_ident ~loc { loc; txt = lident "Result" $. "ok" })
              [ (Nolabel, eunit ~loc) ]
          in
          let cases = [ case ~lhs ~guard:None ~rhs ] in
          pexp_function_cases ~loc
            (if uniq then cases else List.rev (error_case ~loc :: cases))
        in
        (inj, prj)
    | Pcstr_tuple ts ->
        let ts = List.mapi ~f:(fun i _ -> i) ts in
        let inj =
          let var = var "v" in
          pexp_fun ~loc Nolabel None
            (ppat_tuple ~loc
               (List.map ts ~f:(fun i ->
                    ppat_var ~loc { loc; txt = str "%s%d" var i })))
            (econstruct ctor
               (Some
                  (pexp_tuple ~loc
                     (List.map ts ~f:(fun i -> evar ~loc (str "%s%d" var i))))))
        in
        let prj =
          let var = var "v" in
          let lhs =
            pconstruct ctor
              (Some
                 (ppat_tuple ~loc
                    (List.map ts ~f:(fun i ->
                         ppat_var ~loc { loc; txt = str "%s%d" var i }))))
          in
          let rhs =
            pexp_apply ~loc
              (pexp_ident ~loc { loc; txt = lident "Result" $. "ok" })
              [
                ( Nolabel
                , pexp_tuple ~loc
                    (List.map ts ~f:(fun i -> evar ~loc (str "%s%d" var i))) )
              ]
          in
          let cases = [ case ~lhs ~guard:None ~rhs ] in
          pexp_function_cases ~loc
            (if uniq then cases else List.rev (error_case ~loc :: cases))
        in
        (inj, prj)
    | Pcstr_record fields ->
        let inj =
          pexp_fun ~loc Nolabel None
            (ppat_tuple ~loc
               (List.map fields ~f:(fun { pld_name; _ } ->
                    ppat_var ~loc { loc; txt = str "%s" pld_name.txt })))
            (econstruct ctor
               (Some
                  (pexp_record ~loc
                     (List.map fields ~f:(fun { pld_name; _ } ->
                          let ident = { txt = lident pld_name.txt; loc } in
                          (ident, pexp_ident ~loc ident)))
                     None)))
        in
        let prj =
          let lhs =
            pconstruct ctor
              (Some
                 (ppat_record ~loc
                    (List.map fields ~f:(fun { pld_name; _ } ->
                         let ident = { txt = lident pld_name.txt; loc } in
                         (ident, ppat_var ~loc pld_name)))
                    Closed))
          in
          let rhs =
            pexp_apply ~loc
              (pexp_ident ~loc { loc; txt = lident "Result" $. "ok" })
              [
                ( Nolabel
                , pexp_tuple ~loc
                    (List.map fields ~f:(fun { pld_name; _ } ->
                         evar ~loc pld_name.txt)) )
              ]
          in
          let cases = [ case ~lhs ~guard:None ~rhs ] in
          pexp_function_cases ~loc
            (if uniq then cases else List.rev (error_case ~loc :: cases))
        in
        (inj, prj)
  in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc { loc; txt = str "%s_%s" name ctor_name }
      ; pvb_expr =
          pexp_fun ~loc Nolabel None (punit ~loc)
            (pexp_apply ~loc
               (pexp_ident ~loc { loc; txt = lun $. "prism" })
               [ (Nolabel, inj); (Nolabel, prj) ])
      ; pvb_attributes = []
      ; pvb_loc = loc
      ; pvb_constraint = None
      }
    ]

let lense_intf ~name (ld : label_declaration) =
  let field_name = ld.pld_name.txt in
  let loc = ld.pld_loc in
  psig_value ~loc
    {
      pval_name = { loc; txt = str "%s_%s" name field_name }
    ; pval_type =
        ptyp_constr ~loc
          { loc; txt = lun $. "t" }
          [
            ptyp_constr ~loc { loc; txt = lident name } []
          ; ptyp_constr ~loc { loc; txt = lident name } []
          ; ld.pld_type
          ; ld.pld_type
          ]
    ; pval_attributes = []
    ; pval_loc = loc
    ; pval_prim = []
    }

let prism_intf ~name (ctor : constructor_declaration) =
  let ctor_name = ctor.pcd_name.txt in
  let loc = ctor.pcd_name.loc in
  match ctor.pcd_args with
  | Pcstr_tuple [] ->
      psig_value ~loc
        {
          pval_name = { loc; txt = str "%s_%s" name ctor_name }
        ; pval_type =
            ptyp_constr ~loc
              { loc; txt = lun $. "t" }
              [
                ptyp_constr ~loc { loc; txt = lident name } []
              ; ptyp_constr ~loc { loc; txt = lident name } []
              ; ptyp_constr ~loc { loc; txt = unit } []
              ; ptyp_constr ~loc { loc; txt = unit } []
              ]
        ; pval_attributes = []
        ; pval_loc = loc
        ; pval_prim = []
        }
  | Pcstr_tuple ts ->
      psig_value ~loc
        {
          pval_name = { loc; txt = str "%s_%s" name ctor_name }
        ; pval_type =
            ptyp_constr ~loc
              { loc; txt = lun $. "t" }
              [
                ptyp_constr ~loc { loc; txt = lident name } []
              ; ptyp_constr ~loc { loc; txt = lident name } []
              ; ptyp_tuple ~loc ts
              ; ptyp_tuple ~loc ts
              ]
        ; pval_attributes = []
        ; pval_loc = loc
        ; pval_prim = []
        }
  | Pcstr_record fields ->
      let ts = List.map ~f:(fun { pld_type; _ } -> pld_type) fields in
      psig_value ~loc
        {
          pval_name = { loc; txt = str "%s_%s" name ctor_name }
        ; pval_type =
            ptyp_constr ~loc
              { loc; txt = lun $. "t" }
              [
                ptyp_constr ~loc { loc; txt = lident name } []
              ; ptyp_constr ~loc { loc; txt = lident name } []
              ; ptyp_tuple ~loc ts
              ; ptyp_tuple ~loc ts
              ]
        ; pval_attributes = []
        ; pval_loc = loc
        ; pval_prim = []
        }

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_open; ptype_loc; _ } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Cannot derive optic for such type"
          in
          [ Ast_builder.Default.pstr_extension ~loc ext [] ]
      | { ptype_kind = Ptype_variant ctors; ptype_name; _ } ->
          List.map ctors
            ~f:(prism_impl ~uniq:(List.length ctors = 1) ~name:ptype_name.txt)
      | { ptype_kind = Ptype_record fields; ptype_name; _ } ->
          List.map fields
            ~f:(lense_impl ~uniq:(List.length fields = 1) ~name:ptype_name.txt))
  |> List.concat

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_open; ptype_loc; _ } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Cannot derive optic for such type"
          in
          [ Ast_builder.Default.psig_extension ~loc ext [] ]
      | { ptype_kind = Ptype_variant ctors; ptype_name; _ } ->
          List.map ctors ~f:(prism_intf ~name:ptype_name.txt)
      | { ptype_kind = Ptype_record fields; ptype_name; _ } ->
          List.map fields ~f:(lense_intf ~name:ptype_name.txt))
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let my_deriver =
  Deriving.add "lun" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator

(** Pattern-based lun construction *)

(** Return all the variables in a pattern. *)
let find_vars =
  let o = object
    inherit [_] Ast_traverse.fold as super
    method! pattern p acc =
      match p.ppat_desc with
      | Ppat_var l -> l :: acc
      | _ -> super#pattern p acc
  end
  in fun p -> List.rev (o#pattern p [])

(** Replace all open patterns by a variable. *)
let rename_open_in_pat = object
  inherit Ast_traverse.map as super
  method! pattern p =
    let loc = p.ppat_loc in
    let var = Located.mk ~loc @@ var "b" in
    match p.ppat_desc with
    | Ppat_any ->
      {p with ppat_desc = Ppat_var var}
    | Ppat_record (_, Open) ->
      ppat_alias ~loc p var
    | _ -> super#pattern p
end

(** Replace the given variables by _ in a pattern. *)
let erase_vars_in_pat set = object
  inherit Ast_traverse.map as super
  method! pattern p =
    let loc = p.ppat_loc in
    match p.ppat_desc with
    | Ppat_var l when List.mem l.txt ~set -> ppat_any ~loc
    | _ -> super#pattern p
end

let rec pat_to_constr p =
  let loc = p.ppat_loc in
  match p.ppat_desc with
  (* Should have been removed by {!rename_any_in_pat} *)
  | Ppat_any -> assert false
  (* Valid cases *)
  | Ppat_var v -> pexp_ident ~loc (Located.map_lident v)
  | Ppat_tuple ps ->
    let es = List.map ~f:pat_to_constr ps in
    pexp_tuple ~loc es
  | Ppat_construct (c, None) ->
    pexp_construct ~loc c None
  | Ppat_construct (c, Some (_,p)) ->
    pexp_construct ~loc c (Some (pat_to_constr p))
  | Ppat_variant (c, p) -> 
    pexp_variant ~loc c (Option.map pat_to_constr p)
  | Ppat_record (fields, Closed) ->
    let fields = List.map ~f:(fun (l, p) -> l, pat_to_constr p) fields in
    pexp_record ~loc fields None
  | Ppat_alias ({ppat_desc = Ppat_record (fields, Open); _}, v) ->
    let fields = List.map ~f:(fun (l, p) -> l, pat_to_constr p) fields in
    pexp_record ~loc fields (Some (pexp_ident ~loc @@ Located.map_lident v))
  | Ppat_array ps ->
    let es = List.map ~f:pat_to_constr ps in
    pexp_array ~loc es
  | Ppat_constant c ->
    pexp_constant ~loc c
  | Ppat_constraint (p, ty) ->
    pexp_constraint ~loc (pat_to_constr p) ty
  | Ppat_open (l, p) ->
    pexp_open ~loc (H.Opn.mk ~loc (pmod_ident ~loc l)) (pat_to_constr p)
  | Ppat_alias (p', var) ->
    let var = Located.map_lident var in
    let vars = find_vars p' in
    if List.is_empty vars then
      pexp_ident ~loc var
    else
      let warning =
        attribute_of_warning p'.ppat_loc
          "Variables in a pattern under an alias are ignored"
      in
      H.Exp.ident ~attrs:[warning] ~loc var
  | Ppat_or (p1, _) ->
    pat_to_constr p1
  | Ppat_extension ex ->
    pexp_extension ~loc ex
  (* Unsupported cases *)
  | Ppat_record (_, Open)
  | Ppat_interval (_, _) ->
    pexp_extension ~loc @@
    Location.error_extensionf ~loc
      "This pattern has implicit open variables. Please use an alias to bind its content."
  | Ppat_lazy _
  | Ppat_unpack _
  | Ppat_type _
  | Ppat_exception _ ->
    pexp_extension ~loc @@
    Location.error_extensionf ~loc
      "This feature is not supported in lun patterns"

let optic_of_pattern ~ctxt pat guard =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let l = find_vars pat in
  let prj =
    let rhs =
      pexp_apply ~loc
        (pexp_ident ~loc { loc; txt = lident "Result" $. "ok" })
        [Nolabel,
         pexp_tuple ~loc
           (List.map ~f:(fun v -> pexp_ident ~loc @@ Located.map_lident v) l)]
    in
    let c = case ~lhs:pat ~guard ~rhs in
    H.Exp.function_
      ~loc ~attrs:[attr_set_warning ~loc "-11"]
      [ c ; error_case ~loc ]
  in
  let inj =
    let varoutter = var "s" in
    let p = rename_open_in_pat#pattern pat in
    let e = pat_to_constr p in
    let main_case =
      let lhs = (erase_vars_in_pat @@ List.map ~f:Loc.txt l)#pattern p in
      let rhs = e in
      case ~lhs ~guard ~rhs
    in
    let id_case =
      let lhs = ppat_any ~loc in
      let rhs = evar ~loc varoutter in
      case ~lhs ~guard:None ~rhs
    in
    pexp_function ~loc
      [ pparam_val ~loc Nolabel None @@ pvar ~loc varoutter;
        pparam_val ~loc Nolabel None @@
        ppat_tuple ~loc @@ List.map ~f:(ppat_var ~loc) l ]
      None
      (Pfunction_body
         (H.Exp.match_
            ~loc ~attrs:[attr_set_warning ~loc "-11"]
            (evar ~loc varoutter) [main_case; id_case]))
  in
  pexp_constraint ~loc
    (pexp_fun ~loc Nolabel None (punit ~loc)
       (pexp_apply ~loc
          (pexp_ident ~loc { loc; txt = lun $. "optional" })
          [ (Nolabel, inj); (Nolabel, prj) ]))
    (ptyp_constr ~loc (Located.mk ~loc (lun $. "t")) [ptyp_any ~loc])

let extracter () = Ast_pattern.(ppat __ __)

let optic_pattern =
  Extension.V3.declare "lun"
    Extension.Context.expression
    (extracter ())
    optic_of_pattern

let () =
  Driver.register_transformation ~rules:[
    Context_free.Rule.extension optic_pattern
  ] "lun"
