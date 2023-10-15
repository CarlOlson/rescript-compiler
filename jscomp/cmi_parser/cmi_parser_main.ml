module StringMap = Map.Make(String)

type t = polymorphic_variant_type StringMap.t
and polymorphic_variant_type = {
  constructors: string list;
}

let rec process_cmi_info ({ cmi_sign; _ }: Cmi_format.cmi_infos) =
  List.fold_right (fun sign_item t -> process_signature_item t sign_item)
    cmi_sign StringMap.empty

and process_signature_item (t: t) (sign_item: Types.signature_item) =
  match sign_item with
   | Types.Sig_type ({ name; _ }, type_decl, _) -> process_type_decl t name type_decl
   | Types.Sig_value (_, _)
   | Types.Sig_typext (_, _, _)
   | Types.Sig_module (_, _, _)
   | Types.Sig_modtype (_, _)
   | Types.Sig_class ()
   | Types.Sig_class_type (_, _, _) -> t

and process_type_decl (t: t) name (type_decl: Types.type_declaration) =
  match type_decl with
   | { type_kind = Type_abstract; type_manifest = Some type_expr; _ } ->
      StringMap.add name { constructors = get_polymorphic_variant_constructors name type_expr; } t
   | _ ->
      Printf.eprintf "[WARN] Non-polymorphic type %s\n" name;
      t

and get_polymorphic_variant_constructors type_name (type_expr: Types.type_expr) =
  match type_expr.desc with
  | Types.Tvariant { row_fields; _ } -> List.map (get_row_field_label type_name) row_fields
  | _ -> failwith "expected polymorphic variant"

and get_row_field_label type_name ((label, row_field): (string * Types.row_field)) =
  match row_field with
  | Types.Rpresent (Some _) ->
     failwith (Printf.sprintf "%s@%s: unexpected polymorphic variant with payload" type_name label)
  | Types.Rpresent None
  | Types.Reither (_, _, _, _)
  | Types.Rabsent -> ();
  label

and print_t (t: t) =
  StringMap.iter (fun name type_info ->
      Printf.printf "let %s = [\n" name;
      List.iter (fun name -> Printf.printf "  | %s\n" name) type_info.constructors;
      Printf.printf "]\n\n"
    ) t

let () =
  match Sys.argv with
  | [| _; filename |] ->
     let t = process_cmi_info (Cmi_format.read_cmi filename) in
     print_t t
  | _ -> failwith "expect one argument"
