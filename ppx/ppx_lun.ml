open Ppxlib
module List = ListLabels
open Ast_builder.Default

let ( $. ) l x = Longident.Ldot (l, x)
let lun = Longident.Lident "Lun"
let unit = lident "unit"

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

let var ?(len = 6) prefix = Fmt.str "%s%s" prefix (random_string ~len)

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
        pvb_pat = ppat_var ~loc { loc; txt = Fmt.str "%s_%s" name field_name }
      ; pvb_expr =
          pexp_fun ~loc Nolabel None (punit ~loc)
            (pexp_apply ~loc
               (pexp_ident ~loc { loc; txt = lun $. "lense" })
               [ (Nolabel, prj); (Nolabel, inj) ])
      ; pvb_attributes = []
      ; pvb_loc = loc
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
          pexp_function ~loc
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
                    ppat_var ~loc { loc; txt = Fmt.str "%s%d" var i })))
            (econstruct ctor
               (Some
                  (pexp_tuple ~loc
                     (List.map ts ~f:(fun i -> evar ~loc (Fmt.str "%s%d" var i))))))
        in
        let prj =
          let var = var "v" in
          let lhs =
            pconstruct ctor
              (Some
                 (ppat_tuple ~loc
                    (List.map ts ~f:(fun i ->
                         ppat_var ~loc { loc; txt = Fmt.str "%s%d" var i }))))
          in
          let rhs =
            pexp_apply ~loc
              (pexp_ident ~loc { loc; txt = lident "Result" $. "ok" })
              [
                ( Nolabel
                , pexp_tuple ~loc
                    (List.map ts ~f:(fun i -> evar ~loc (Fmt.str "%s%d" var i)))
                )
              ]
          in
          let cases = [ case ~lhs ~guard:None ~rhs ] in
          pexp_function ~loc
            (if uniq then cases else List.rev (error_case ~loc :: cases))
        in
        (inj, prj)
    | Pcstr_record _ -> assert false
  in
  pstr_value ~loc Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc { loc; txt = Fmt.str "%s_%s" name ctor_name }
      ; pvb_expr =
          pexp_fun ~loc Nolabel None (punit ~loc)
            (pexp_apply ~loc
               (pexp_ident ~loc { loc; txt = lun $. "prism" })
               [ (Nolabel, inj); (Nolabel, prj) ])
      ; pvb_attributes = []
      ; pvb_loc = loc
      }
    ]

let lense_intf ~name (ld : label_declaration) =
  let field_name = ld.pld_name.txt in
  let loc = ld.pld_loc in
  psig_value ~loc
    {
      pval_name = { loc; txt = Fmt.str "%s_%s" name field_name }
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
          pval_name = { loc; txt = Fmt.str "%s_%s" name ctor_name }
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
          pval_name = { loc; txt = Fmt.str "%s_%s" name ctor_name }
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
  | _ -> assert false

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_open; ptype_loc; _ } ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc
              "Cannot derive lense for non record types"
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
              "Cannot derive lense for non record types"
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
