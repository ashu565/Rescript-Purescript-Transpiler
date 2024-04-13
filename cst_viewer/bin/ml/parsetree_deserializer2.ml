open Parsetree

let constant_of_yojson (json : Yojson.Safe.t) : Parsetree.constant =
  match json with
  | `Assoc [("tag", `String "PconstInteger"); ("contents", `List [`String value; suffix])] ->
    let suffix_value =
      match suffix with
      | `Null -> None
      | `String value -> Some (Char.chr (int_of_string value))
      | _ -> failwith "Invalid JSON format for suffix of PconstInteger"
    in
    Pconst_integer (value, suffix_value)
  | `Assoc [("tag", `String "PconstChar"); ("contents", `Int value)] -> Pconst_char value
  | `Assoc [("tag", `String "PconstString"); ("contents", `List [`String value; delim])] ->
    let delim_value =
      match delim with
      | `Null -> None
      | `String value -> Some value
      | _ -> failwith "Invalid JSON format for delimiter of PconstString"
    in
    Pconst_string (value, delim_value)
  | `Assoc [("tag", `String "PconstFloat"); ("contents", `List [`String value; suffix])] ->
    let suffix_value =
      match suffix with
      | `Null -> None
      | `String value -> Some (Char.chr (int_of_string value))
      | _ -> failwith "Invalid JSON format for suffix of PconstFloat"
    in
    Pconst_float (value, suffix_value)
  | _ -> failwith "Invalid JSON format for Parsetree.constant"

let rec attribute_of_yojson (json : Yojson.Safe.t) : attribute =
  match json with
  | `List [loc_json; payload_json] ->
    let loc = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc_json in
    let payload = payload_of_yojson payload_json in
    (loc, payload)
  | _ -> failwith "Invalid JSON format for attribute"

and extension_of_yojson (json : Yojson.Safe.t) : extension =
  match json with
  | `List [loc_json; payload_json] ->
    let loc = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc_json in
    let payload = payload_of_yojson payload_json in
    (loc, payload)
  | _ -> failwith "Invalid JSON format for extension"

and attributes_of_yojson (json : Yojson.Safe.t) : attributes =
  match json with
  | `List jsons -> List.map attribute_of_yojson jsons
  | _ -> failwith "Invalid JSON format for attributes"

and payload_of_yojson (json : Yojson.Safe.t) : payload =
  match json with
  | `Assoc [("tag", `String "PStr"); ("contents", jsons)] ->
    PStr (structure_of_yojson jsons)
  | `Assoc [("tag", `String "PSig"); ("contents", jsons)] ->
    PSig (signature_of_yojson jsons)
  | `Assoc [("tag", `String "PTyp"); ("contents", json)] ->
    PTyp (core_type_of_yojson json)
  | `Assoc [("tag", `String "PPat"); ("contents", json)] ->
    (match json with
     | `List [pat_json; option_expr_json] ->
       let pat = pattern_of_yojson pat_json in
       let option_expr =
         match option_expr_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson option_expr_json)
       in
       PPat (pat, option_expr)
     | _ -> failwith "Invalid JSON format for PPat")
  | _ -> failwith "Invalid JSON format for payload"

and core_type_of_yojson (json : Yojson.Safe.t) : core_type =
  match json with
  | `Assoc _ ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "CoreType" ->
      {
        ptyp_desc = Yojson.Safe.Util.member "ptypDesc" json |> core_type_desc_of_yojson;
        ptyp_loc = Yojson.Safe.Util.member "ptypLoc" json |> Location_deserializer.t_of_yojson;
        ptyp_attributes = Yojson.Safe.Util.member "ptypAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for core_type: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for core_type"

and core_type_desc_of_yojson (json : Yojson.Safe.t) : core_type_desc = 
  match json with
  | `Assoc [("tag", `String "PtypAny")] -> Ptyp_any
  | `Assoc [("tag", `String "PtypVar"); ("contents", `String value)] -> Ptyp_var value
  | `Assoc [("tag", `String "PtypArrow"); ("contents", jsons)] ->
    (match jsons with
     | `List [label; core_type1_json; core_type2_json] ->
       let label = Asttypes_deserializer.arg_label_of_yojson label in
       let core_type1 = core_type_of_yojson core_type1_json in
       let core_type2 = core_type_of_yojson core_type2_json in
       Ptyp_arrow (label, core_type1, core_type2)
     | _ -> failwith "Invalid JSON format for Ptyp_arrow")
  | `Assoc [("tag", `String "PtypTuple"); ("contents", jsons)] ->
    (match jsons with
     | `List core_types_json -> Ptyp_tuple (List.map core_type_of_yojson core_types_json)
     | _ -> failwith "Invalid JSON format for Ptyp_tuple")
  | `Assoc [("tag", `String "PtypConstr"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; (`List core_types_json)] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Ptyp_constr (longident_loc, core_types)
     | _ -> failwith "Invalid JSON format for Ptyp_constr")
  | `Assoc [("tag", `String "PtypObject"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List fields); closed_flag] ->
       let fields = List.map object_field_of_yojson fields in
       let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag in
       Ptyp_object (fields, closed_flag)
     | _ -> failwith "Invalid JSON format for Ptyp_object")
  | `Assoc [("tag", `String "PtypClass"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; (`List core_types_json)] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Ptyp_class (longident_loc, core_types)
     | _ -> failwith "Invalid JSON format for Ptyp_class")
  | `Assoc [("tag", `String "PtypAlias"); ("contents", jsons)] ->
    (match jsons with
     | `List [core_type_json; `String value] ->
       let core_type = core_type_of_yojson core_type_json in
       Ptyp_alias (core_type, value)
     | _ -> failwith "Invalid JSON format for Ptyp_alias")
  | `Assoc [("tag", `String "PtypVariant"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List row_fields); closed_flag; labels] ->
       let row_fields = List.map row_field_of_yojson row_fields in
       let closed_flag = Asttypes_deserializer.closed_flag_of_yojson closed_flag in
       let labels_d = match labels with
          | `Null -> None
          | `List labels_list -> Some (List.map Asttypes_deserializer.label_of_yojson labels_list)
          | _ -> failwith "Invalid JSON format for labels of Ptyp_variant" in
       Ptyp_variant (row_fields, closed_flag, labels_d)
     | _ -> failwith "Invalid JSON format for Ptyp_variant")
  | `Assoc [("tag", `String "PtypPoly"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List strings); core_type_json] ->
       let stringss = List.map (Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x)) strings in
       let core_type = core_type_of_yojson core_type_json in
       Ptyp_poly (stringss, core_type)
     | _ -> failwith "Invalid JSON format for Ptyp_poly")
  | _ -> failwith "Invalid JSON format for core_type_desc"
  
and package_type_of_yojson (json : Yojson.Safe.t) : package_type =
    match json with
    | `List [loc_json; items_json] ->
      let loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson loc_json in
      let items = 
        match items_json with
        | `List l ->
          List.map (fun item ->
            match item with
            | `List [loc_item_json; core_type_json] ->
              let loc_item = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson loc_item_json in
              let core_type = core_type_of_yojson core_type_json in
              (loc_item, core_type)
            | _ -> failwith "Invalid JSON format for package_type item"
          ) l
        | _ -> failwith "Invalid JSON format for package_type items"
      in
      (loc, items)
    | _ -> failwith "Invalid JSON format for package_type"

and row_field_of_yojson (json : Yojson.Safe.t) : row_field =
  match json with
  | `Assoc [("tag", `String "Rtag"); ("contents", jsons)] ->
    (match jsons with
     | `List [label_loc_json; attributes_json; (`Bool bool); (`List core_types_json)] ->
       let label_loc = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson label_loc_json in
       let attributes = attributes_of_yojson attributes_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Rtag (label_loc, attributes, bool, core_types)
     | _ -> failwith "Invalid JSON format for Rtag")
  | `Assoc [("tag", `String "Rinherit"); ("contents", json)] ->
    (match json with
     | `List [core_type_json] ->
       let core_type = core_type_of_yojson core_type_json in
       Rinherit core_type
     | _ -> failwith "Invalid JSON format for Rinherit")
  | _ -> failwith "Invalid JSON format for row_field"

and object_field_of_yojson (json : Yojson.Safe.t) : object_field =
  match json with
  | `Assoc [("tag", `String "Otag"); ("contents", jsons)] ->
    (match jsons with
     | `List [label_loc_json; attributes_json; core_type_json] ->
       let label_loc = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson label_loc_json in
       let attributes = attributes_of_yojson attributes_json in
       let core_type = core_type_of_yojson core_type_json in
       Otag (label_loc, attributes, core_type)
     | _ -> failwith "Invalid JSON format for Otag")
  | `Assoc [("tag", `String "Oinherit"); ("contents", json)] ->
    (match json with
     | `List [core_type_json] ->
       let core_type = core_type_of_yojson core_type_json in
       Oinherit core_type
     | _ -> failwith "Invalid JSON format for Oinherit")
  | _ -> failwith "Invalid JSON format for object_field"

and pattern_of_yojson (json : Yojson.Safe.t) : pattern =
  match json with
  | `Assoc _ ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "Pattern" ->
      {
        ppat_desc = Yojson.Safe.Util.member "ppatDesc" json |> pattern_desc_of_yojson;
        ppat_loc = Yojson.Safe.Util.member "ppatLoc" json |> Location_deserializer.t_of_yojson;
        ppat_attributes = Yojson.Safe.Util.member "ppatAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for pattern: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for pattern"


and pattern_desc_of_yojson (json : Yojson.Safe.t) : pattern_desc =
  match json with
  | `Assoc [("tag", `String "PpatAny")] -> Ppat_any
  | `Assoc [("tag", `String "PpatVar"); ("contents", json)] -> Ppat_var (Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) json)
  | `Assoc [("tag", `String "PpatAlias"); ("contents", jsons)] ->
    (match jsons with
     | `List [pattern_json; loc] ->
       let pattern = pattern_of_yojson pattern_json in
       let loc_x = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       Ppat_alias (pattern, loc_x)
     | _ -> failwith "Invalid JSON format for Ppat_alias")
  | `Assoc [("tag", `String "PpatConstant"); ("contents", json)] -> Ppat_constant (constant_of_yojson json)
  | `Assoc [("tag", `String "PpatInterval"); ("contents", jsons)] ->
    (match jsons with
     | `List [constant1_json; constant2_json] ->
       let constant1 = constant_of_yojson constant1_json in
       let constant2 = constant_of_yojson constant2_json in
       Ppat_interval (constant1, constant2)
     | _ -> failwith "Invalid JSON format for Ppat_interval")
  | `Assoc [("tag", `String "PpatTuple"); ("contents", jsons)] ->
    (match jsons with
     | `List patterns_json -> Ppat_tuple (List.map pattern_of_yojson patterns_json)
     | _ -> failwith "Invalid JSON format for Ppat_tuple")
  | `Assoc [("tag", `String "PpatConstruct"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; option_pattern_json] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let option_pattern =
         match option_pattern_json with
         | `Null -> None
         | _ -> Some (pattern_of_yojson option_pattern_json)
       in
       Ppat_construct (longident_loc, option_pattern)
     | _ -> failwith "Invalid JSON format for Ppat_construct")
  | `Assoc [("tag", `String "PpatVariant"); ("contents", jsons)] ->
    (match jsons with
     | `List [label_loc_json; option_pattern_json] ->
       let label_loc = Asttypes_deserializer.label_of_yojson label_loc_json in
       let option_pattern =
         match option_pattern_json with
         | `Null -> None
         | _ -> Some (pattern_of_yojson option_pattern_json)
       in
       Ppat_variant (label_loc, option_pattern)
     | _ -> failwith "Invalid JSON format for Ppat_variant")
  | `Assoc [("tag", `String "PpatRecord"); ("contents", jsons)] ->
    (match jsons with
      | `List [`List record_fields_json; cflag] -> 
        let loc_record_fields = List.map (fun json -> 
          match json with
          | `List [longident_loc_json; pattern_json] ->
            let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
            let pattern = pattern_of_yojson pattern_json in
            (longident_loc, pattern)
          | _ -> failwith "Invalid JSON format for record_field"
        ) record_fields_json in
        let closed_flag = Asttypes_deserializer.closed_flag_of_yojson cflag in
        Ppat_record (loc_record_fields, closed_flag)
      | _ -> failwith "Invalid JSON format for Ppat_record")
  | `Assoc [("tag", `String "PpatArray"); ("contents", jsons)] ->
    (match jsons with
      | `List patterns_json -> Ppat_array (List.map pattern_of_yojson patterns_json)
      | _ -> failwith "Invalid JSON format for Ppat_array")
  | `Assoc [("tag", `String "PpatOr"); ("contents", jsons)] ->
    (match jsons with
      | `List [pattern1_json; pattern2_json] ->
        let pattern1 = pattern_of_yojson pattern1_json in
        let pattern2 = pattern_of_yojson pattern2_json in
        Ppat_or (pattern1, pattern2)
      | _ -> failwith "Invalid JSON format for Ppat_or")
  | `Assoc [("tag", `String "PpatConstraint"); ("contents", jsons)] ->
    (match jsons with
      | `List [pattern_json; core_type_json] ->
        let pattern = pattern_of_yojson pattern_json in
        let core_type = core_type_of_yojson core_type_json in
        Ppat_constraint (pattern, core_type)
      | _ -> failwith "Invalid JSON format for Ppat_constraint")
  | `Assoc [("tag", `String "PpatType"); ("contents", value)] -> Ppat_type (Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson value)
  | `Assoc [("tag", `String "PpatLazy"); ("contents", json)] -> Ppat_lazy (pattern_of_yojson json)
  | `Assoc [("tag", `String "PpatUnpack"); ("contents", value)] -> Ppat_unpack (Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) value)
  | `Assoc [("tag", `String "PpatException"); ("contents", json)] -> Ppat_exception (pattern_of_yojson json)
  | `Assoc [("tag", `String "PpatExtension"); ("contents", json)] -> Ppat_extension (extension_of_yojson json)
  | `Assoc [("tag", `String "PpatOpen"); ("contents", jsons)] -> 
    (match jsons with
      | `List [loc; pattern] -> 
          let loc_type = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson loc in
          let pattern_type = pattern_of_yojson pattern in
        Ppat_open (loc_type, pattern_type)
      | _ -> failwith "Invalid JSON format for Ppat_open")
  | _ -> failwith "Invalid JSON format for pattern_desc"

and expression_of_yojson (json : Yojson.Safe.t) : expression =
  match json with
  | `Assoc _ ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "Expression" ->
      {
        pexp_desc = Yojson.Safe.Util.member "pexpDesc" json |> expression_desc_of_yojson;
        pexp_loc = Yojson.Safe.Util.member "pexpLoc" json |> Location_deserializer.t_of_yojson;
        pexp_attributes = Yojson.Safe.Util.member "pexpAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for expression: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for expression"

and expression_desc_of_yojson (json : Yojson.Safe.t) : expression_desc = 
  match json with
  | `Assoc [("tag", `String "PexpIdent"); ("contents", json)] -> Pexp_ident (Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson json)
  | `Assoc [("tag", `String "PexpConstant"); ("contents", json)] -> Pexp_constant (constant_of_yojson json)
  |  `Assoc [("tag", `String "PexpLet"); ("contents", jsons)] ->
    (match jsons with
     | `List [rec_flag; `List value_bindings_json_arr; expression_json] ->
       let rec_flag = Asttypes_deserializer.rec_flag_of_yojson rec_flag in
       let value_bindings = List.map value_binding_of_yojson value_bindings_json_arr in
       let expression = expression_of_yojson expression_json in
       Pexp_let (rec_flag, value_bindings, expression)
     | _ -> failwith "Invalid JSON format for Pexp_let")
  | `Assoc [("tag", `String "PexpFunction"); ("contents", `List jsons)] -> Pexp_function (List.map case_of_yojson jsons)
  | `Assoc [("tag", `String "PexpFun"); ("contents", jsons)] ->
    (match jsons with
     | `List [label; default_expr_json; pattern_json; expression_json] ->
       let label = Asttypes_deserializer.arg_label_of_yojson label in
       let default_expr =
         match default_expr_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson default_expr_json)
       in
       let pattern = pattern_of_yojson pattern_json in
       let expression = expression_of_yojson expression_json in
       Pexp_fun (label, default_expr, pattern, expression)
     | _ -> failwith "Invalid JSON format for Pexp_fun")
  | `Assoc [("tag", `String "PexpApply"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; `List exp_arr] ->
       let expression = expression_of_yojson expression_json in
       let exp = List.map (fun json -> 
        match json with
        | `List [arg; exp] ->
            let arg_type = Asttypes_deserializer.arg_label_of_yojson arg in
            let exp_type = expression_of_yojson exp in
            (arg_type, exp_type)
        | _ -> failwith "Invalid JSON format for Pexp_apply") exp_arr in
       Pexp_apply (expression,exp)
     | _ -> failwith "Invalid JSON format for Pexp_apply")
  | `Assoc [("tag", `String "PexpMatch"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; `List case_jsons] ->
       let expression = expression_of_yojson expression_json in
       let cases = List.map case_of_yojson case_jsons in
       Pexp_match (expression, cases)
     | _ -> failwith "Invalid JSON format for Pexp_match")
  | `Assoc [("tag", `String "PexpTry"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; `List case_jsons] ->
       let expression = expression_of_yojson expression_json in
       let cases = List.map case_of_yojson case_jsons in
       Pexp_try (expression, cases)
     | _ -> failwith "Invalid JSON format for Pexp_try")
  | `Assoc [("tag", `String "PexpTuple"); ("contents", jsons)] ->
    (match jsons with
     | `List expressions_json -> Pexp_tuple (List.map expression_of_yojson expressions_json)
     | _ -> failwith "Invalid JSON format for Pexp_tuple")
  | `Assoc [("tag", `String "PexpConstruct"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; option_expression_json] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let option_expression =
         match option_expression_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson option_expression_json)
       in
       Pexp_construct (longident_loc, option_expression)
     | _ -> failwith "Invalid JSON format for Pexp_construct")
  | `Assoc [("tag", `String "PexpVariant"); ("contents", jsons)] ->
    (match jsons with
     | `List [label_loc_json; option_expression_json] ->
       let label_loc = Asttypes_deserializer.label_of_yojson label_loc_json in
       let option_expression =
         match option_expression_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson option_expression_json)
       in
       Pexp_variant (label_loc, option_expression)
     | _ -> failwith "Invalid JSON format for Pexp_variant")
  | `Assoc [("tag", `String "PexpRecord"); ("contents", jsons)] ->
    (match jsons with
     | `List [(`List record_fields_json); option_expression_json] ->
       let record_fields = List.map (fun json ->
         match json with
         | `List [longident_loc_json; expression_json] ->
           let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
           let expression = expression_of_yojson expression_json in
           (longident_loc, expression)
         | _ -> failwith "Invalid JSON format for record_field"
       ) record_fields_json in
       let option_expression =
         match option_expression_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson option_expression_json)
       in
       Pexp_record (record_fields, option_expression)
     | _ -> failwith "Invalid JSON format for Pexp_record")
  | `Assoc [("tag", `String "PexpField"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; longident_loc_json] ->
       let expression = expression_of_yojson expression_json in
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       Pexp_field (expression, longident_loc)
     | _ -> failwith "Invalid JSON format for Pexp_field")
  | `Assoc [("tag", `String "PexpSetField"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression1_json; longident_loc_json; expression2_json] ->
       let expression1 = expression_of_yojson expression1_json in
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let expression2 = expression_of_yojson expression2_json in
       Pexp_setfield (expression1, longident_loc, expression2)
     | _ -> failwith "Invalid JSON format for Pexp_setfield")
  | `Assoc [("tag", `String "PexpArray"); ("contents", jsons)] ->
    (match jsons with
     | `List expressions_json -> Pexp_array (List.map expression_of_yojson expressions_json)
     | _ -> failwith "Invalid JSON format for Pexp_array")
  | `Assoc [("tag", `String "PexpIfThenElse"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression1_json; expression2_json; expression3_json] ->
       let expression1 = expression_of_yojson expression1_json in
       let expression2 = expression_of_yojson expression2_json in
       let expression3 = match expression3_json with
         | `Null -> None
         | _ -> Some (expression_of_yojson expression3_json) in
       Pexp_ifthenelse (expression1, expression2, expression3)
     | _ -> failwith "Invalid JSON format for Pexp_ifthenelse")
  | `Assoc [("tag", `String "PexpSequence"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression1_json; expression2_json] ->
       let expression1 = expression_of_yojson expression1_json in
       let expression2 = expression_of_yojson expression2_json in
       Pexp_sequence (expression1, expression2)
     | _ -> failwith "Invalid JSON format for Pexp_sequence")
  | `Assoc [("tag", `String "PexpWhile"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression1_json; expression2_json] ->
       let expression1 = expression_of_yojson expression1_json in
       let expression2 = expression_of_yojson expression2_json in
       Pexp_while (expression1, expression2)
     | _ -> failwith "Invalid JSON format for Pexp_while")
  | `Assoc [("tag", `String "PexpFor"); ("contents", jsons)] ->
    (match jsons with
     | `List [pattern_json; expression1_json; expression2_json; direction; expression3_json] ->
       let pattern = pattern_of_yojson pattern_json in
       let expression1 = expression_of_yojson expression1_json in
       let expression2 = expression_of_yojson expression2_json in
       let direction = Asttypes_deserializer.direction_flag_of_yojson direction in
       let expression3 = expression_of_yojson expression3_json in
       Pexp_for (pattern, expression1, expression2, direction, expression3)
     | _ -> failwith "Invalid JSON format for Pexp_for")
  | `Assoc [("tag", `String "PexpConstraint"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; core_type_json] ->
       let expression = expression_of_yojson expression_json in
       let core_type = core_type_of_yojson core_type_json in
       Pexp_constraint (expression, core_type)
     | _ -> failwith "Invalid JSON format for Pexp_constraint")
  | `Assoc [("tag", `String "PexpCoerce"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; option_core_type1_json; core_type2_json] ->
       let expression = expression_of_yojson expression_json in
       let option_core_type1 =
         match option_core_type1_json with
         | `Null -> None
         | _ -> Some (core_type_of_yojson option_core_type1_json)
       in
       let core_type2 = core_type_of_yojson core_type2_json in
       Pexp_coerce (expression, option_core_type1, core_type2)
     | _ -> failwith "Invalid JSON format for Pexp_coerce")
  | `Assoc [("tag", `String "PexpSend"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; label_loc_json] ->
       let expression = expression_of_yojson expression_json in
       let label_loc = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson label_loc_json in
       Pexp_send (expression, label_loc)
     | _ -> failwith "Invalid JSON format for Pexp_send")
  | `Assoc [("tag", `String "PexpNew"); ("contents", json)] -> Pexp_new (Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson json)
  | `Assoc [("tag", `String "PexpSetInstVar"); ("contents", jsons)] ->
    (match jsons with
     | `List [label_loc_json; expression_json] ->
       let label_loc = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson label_loc_json in
       let expression = expression_of_yojson expression_json in
       Pexp_setinstvar (label_loc, expression)
     | _ -> failwith "Invalid JSON format for Pexp_setinstvar")
  | `Assoc [("tag", `String "PexpOverride"); ("contents", jsons)] ->
    (match jsons with
      | `List [contents] ->
        (match contents with
          | `List expression_arr -> Pexp_override (List.map (fun json -> 
              match json with
              | `List [loc; exp] -> 
                let loc_type = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson loc in
                let exp_type = expression_of_yojson exp in
                (loc_type, exp_type)
              | _ -> failwith "Invalid JSON format for Pexp_override_list"
              ) expression_arr)
          | _ -> failwith "Invalid JSON format for Pexp_override")
      | _ -> failwith "Invalid JSON format for Pexp_override"
    )
  | `Assoc [("tag", `String "PexpLetModule"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; module_expr_json; expression_json] ->
       let loc_type = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       let module_expr = module_expr_of_yojson module_expr_json in
       let expression = expression_of_yojson expression_json in
       Pexp_letmodule (loc_type, module_expr, expression)
     | _ -> failwith "Invalid JSON format for Pexp_letmodule")
  | `Assoc [("tag", `String "PexpLetexception"); ("contents", jsons)] ->
    (match jsons with
     | `List [extension_constructor_json; expression_json] ->
       let extension_constructor = extension_constructor_of_yojson extension_constructor_json in
       let expression = expression_of_yojson expression_json in
       Pexp_letexception (extension_constructor, expression)
     | _ -> failwith "Invalid JSON format for Pexp_letexception")
  | `Assoc [("tag", `String "PexpAssert"); ("contents", json)] -> Pexp_assert (expression_of_yojson json)
  | `Assoc [("tag", `String "PexpLazy"); ("contents", json)] -> Pexp_lazy (expression_of_yojson json)
  | `Assoc [("tag", `String "PexpPoly"); ("contents", jsons)] ->
    (match jsons with
     | `List [expression_json; core_type_json] ->
       let expression = expression_of_yojson expression_json in
       let core_type = 
        match core_type_json with
        | `Null -> None
        | _ -> Some (core_type_of_yojson core_type_json) in
       Pexp_poly (expression, core_type)
     | _ -> failwith "Invalid JSON format for Pexp_poly")
  | `Assoc [("tag", `String "PexpObject"); ("contents", json)] -> Pexp_object (class_structure_of_yojson json)
  | `Assoc [("tag", `String "PexpNewtype"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; expression_json] ->
       let loc_type = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       let expression = expression_of_yojson expression_json in
       Pexp_newtype (loc_type, expression)
     | _ -> failwith "Invalid JSON format for Pexp_newtype")
  | `Assoc [("tag", `String "PexpPack"); ("contents", json)] -> Pexp_pack (module_expr_of_yojson json)
  | `Assoc [("tag", `String "PexpOpen")] -> 
    (match json with
      | `List [loc; longident_loc; expression] -> 
        let loc_type = Asttypes_deserializer.override_flag_of_yojson loc in
        let longident_loc_type = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc in
        let expression_type = expression_of_yojson expression in
        Pexp_open (loc_type, longident_loc_type, expression_type)
      | _ -> failwith "Invalid JSON format for Pexp_open")
  | `Assoc [("tag", `String "PexpExtension"); ("contents", json)] -> Pexp_extension (extension_of_yojson json)
  | `Assoc [("tag", `String "PexpUnreachable")] -> Pexp_unreachable
  | _ -> failwith "Invalid JSON format for expression_desc"

and case_of_yojson (json : Yojson.Safe.t) : case =
  match json with
  | `Assoc _ ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "Case" ->
      {
        pc_lhs = Yojson.Safe.Util.member "pcLhs" json |> pattern_of_yojson;
        pc_guard = Yojson.Safe.Util.member "pcGuard" json |> (fun x -> match x with
          | `Null -> None
          | _ -> Some (expression_of_yojson x)
        );
        pc_rhs = Yojson.Safe.Util.member "pcRhs" json |> expression_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for case: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for case"


and value_description_of_yojson (json : Yojson.Safe.t) : value_description = 
  match json with
  | `Assoc _ ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ValueDescription" ->
      {
        pval_name = Yojson.Safe.Util.member "pvalName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pval_type = Yojson.Safe.Util.member "pvalType" json |> core_type_of_yojson;
        pval_prim = Yojson.Safe.Util.member "pvalPrim" json |> (fun x -> match x with
          | `List x -> List.map Yojson.Safe.Util.to_string x
          | _ -> failwith "Invalid JSON format for pval_prim"
        );
        pval_attributes = Yojson.Safe.Util.member "pvalAttributes" json |> attributes_of_yojson;
        pval_loc = Yojson.Safe.Util.member "pvalLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for value_description: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for value_description"

and type_declaration_of_yojson (json : Yojson.Safe.t) : type_declaration = 
  match json with
  | `Assoc _ ->    
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "TypeDeclaration" ->
      {
        ptype_name = Yojson.Safe.Util.member "ptypeName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        ptype_params = Yojson.Safe.Util.member "ptypeParams" json |> (fun x -> match x with
          | `List x_ -> List.map (fun y ->
            match y with
            | `List [t; v] -> (core_type_of_yojson t, Asttypes_deserializer.variance_of_yojson v)
            | _ -> failwith "Invalid JSON format for ptype_params"
            ) x_
          | _ -> failwith "Invalid JSON format for ptype_params"
        );
        ptype_cstrs = Yojson.Safe.Util.member "ptypeCstrs" json |> (fun x -> match x with
          | `List x -> List.map (fun y ->
            match y with
            | `List [t1; t2; l] -> (core_type_of_yojson t1, core_type_of_yojson t2, Location_deserializer.t_of_yojson l)
            | _ -> failwith "Invalid JSON format for ptype_cstrs") x
          | _ -> failwith "Invalid JSON format for ptype_cstrs"
        );
        ptype_kind = Yojson.Safe.Util.member "ptypeKind" json |> type_kind_of_yojson;
        ptype_private = Yojson.Safe.Util.member "ptypePrivate" json |> Asttypes_deserializer.private_flag_of_yojson;
        ptype_manifest = Yojson.Safe.Util.member "ptypeManifest" json |> (fun x -> match x with
          | `Null -> None
          | _ -> Some (core_type_of_yojson x)
        );
        ptype_attributes = Yojson.Safe.Util.member "ptypeAttributes" json |> attributes_of_yojson;
        ptype_loc = Yojson.Safe.Util.member "ptypeLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for type_declaration: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for type_declaration"


and type_kind_of_yojson (json : Yojson.Safe.t) : type_kind = 
  match json with
  | `Assoc [("tag", `String "Ptype_abstract")] -> Ptype_abstract
  | `Assoc [("tag", `String "Ptype_variant"); ("contents", jsons)] ->
    (match jsons with
     | `List [`List labels] -> Ptype_variant (List.map constructor_declaration_of_yojson labels)
     | _ -> failwith "Invalid JSON format for Ptype_variant")
  | `Assoc [("tag", `String "Ptype_record"); ("contents", jsons)] ->
    (match jsons with
     | `List [`List labels] -> Ptype_record (List.map label_declaration_of_yojson labels)
     | _ -> failwith "Invalid JSON format for Ptype_record")
  | `Assoc [("tag", `String "Ptype_open")] -> Ptype_open
  | _ -> failwith "Invalid JSON format for type_kind"

and label_declaration_of_yojson (json : Yojson.Safe.t) : label_declaration =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "LabelDeclaration" ->
      {
        pld_name = Yojson.Safe.Util.member "pldName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pld_mutable = Yojson.Safe.Util.member "pldMutable" json |> Asttypes_deserializer.mutable_flag_of_yojson;
        pld_type = Yojson.Safe.Util.member "pldType" json |> core_type_of_yojson;
        pld_loc = Yojson.Safe.Util.member "pldLoc" json |> Location_deserializer.t_of_yojson;
        pld_attributes = Yojson.Safe.Util.member "pldAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for label_declaration: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for label_declaration"


and constructor_declaration_of_yojson (json : Yojson.Safe.t) : constructor_declaration =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ConstructorDeclaration" ->
      {
        pcd_name = Yojson.Safe.Util.member "pcdName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pcd_args = Yojson.Safe.Util.member "pcdArgs" json |> constructor_arguments_of_yojson;
        pcd_res = Yojson.Safe.Util.member "pcdRes" json |> (fun x -> match x with
          | `Null -> None
          | _ -> Some (core_type_of_yojson x)
        );
        pcd_loc = Yojson.Safe.Util.member "pcdLoc" json |> Location_deserializer.t_of_yojson;
        pcd_attributes = Yojson.Safe.Util.member "pcdAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for constructor_declaration: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for constructor_declaration"



and constructor_arguments_of_yojson (json : Yojson.Safe.t) : constructor_arguments =
  match json with
  | `Assoc [("tag", `String "Pcstr_tuple"); ("contents", jsons)] ->
    (match jsons with
     | `List core_types_json -> Pcstr_tuple (List.map core_type_of_yojson core_types_json)
     | _ -> failwith "Invalid JSON format for Pcstr_tuple")
  | `Assoc [("tag", `String "Pcstr_record"); ("contents", jsons)] ->
    (match jsons with
     | `List labels_json -> Pcstr_record (List.map label_declaration_of_yojson labels_json)
     | _ -> failwith "Invalid JSON format for Pcstr_record")
  | _ -> failwith "Invalid JSON format for constructor_arguments"


and type_extension_of_yojson (json : Yojson.Safe.t) : type_extension =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "TypeExtension" ->
      {
        ptyext_path = Yojson.Safe.Util.member "ptyextPath" json |> Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson;
        ptyext_params = Yojson.Safe.Util.member "ptyextParams" json |> (fun x -> match x with
          | `List x_ -> List.map (fun y ->
            match y with
            | `List [t; v] -> (core_type_of_yojson t, Asttypes_deserializer.variance_of_yojson v)
            | _ -> failwith "Invalid JSON format for ptyext_params"
            ) x_
          | _ -> failwith "Invalid JSON format for ptyext_params"
        );
        ptyext_constructors = Yojson.Safe.Util.member "ptyextConstructors" json |> (fun x -> 
          match x with
          | `List cons -> List.map extension_constructor_of_yojson cons
          | _ -> failwith "Invalid JSON format for ptyext_constructors"
        );
        ptyext_private = Yojson.Safe.Util.member "ptyextPrivate" json |> Asttypes_deserializer.private_flag_of_yojson;
        ptyext_attributes = Yojson.Safe.Util.member "ptyextAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for type_extension: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for type_extension"


and extension_constructor_of_yojson (json : Yojson.Safe.t) : extension_constructor =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ExtensionConstructor" ->
      {
        pext_name = Yojson.Safe.Util.member "pextName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pext_kind = Yojson.Safe.Util.member "pextKind" json |> extension_constructor_kind_of_yojson;
        pext_loc = Yojson.Safe.Util.member "pextLoc" json |> Location_deserializer.t_of_yojson;
        pext_attributes = Yojson.Safe.Util.member "pextAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for extension_constructor: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for extension_constructor"


and extension_constructor_kind_of_yojson (json : Yojson.Safe.t) : extension_constructor_kind =
  match json with
  | `Assoc [("tag", `String "Pext_decl"); ("contents", jsons)] ->
    (match jsons with
     | `List [ca; ct] -> Pext_decl (constructor_arguments_of_yojson ca, (match ct with
        | `Null -> None
        | _ -> Some (core_type_of_yojson ct)
     ))
     | _ -> failwith "Invalid JSON format for Pext_decl")
  | `Assoc [("tag", `String "Pext_rebind"); ("contents", json)] -> Pext_rebind (Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson json)
  | _ -> failwith "Invalid JSON format for extension_constructor_kind"

and class_type_of_yojson (json : Yojson.Safe.t) : class_type =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassType" ->
      {
        pcty_desc = Yojson.Safe.Util.member "pctyDesc" json |> class_type_desc_of_yojson;
        pcty_loc = Yojson.Safe.Util.member "pctyLoc" json |> Location_deserializer.t_of_yojson;
        pcty_attributes = Yojson.Safe.Util.member "pctyAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for class_type: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_type"

and class_type_desc_of_yojson (json : Yojson.Safe.t) : class_type_desc =
  match json with
  | `Assoc [("tag", `String "Pcty_constr"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; `List core_types_json] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Pcty_constr (longident_loc, core_types)
     | _ -> failwith "Invalid JSON format for Pcty_constr")
  | `Assoc [("tag", `String "Pcty_signature"); ("contents", json)] -> Pcty_signature (class_signature_of_yojson json)
  | `Assoc [("tag", `String "Pcty_arrow"); ("contents", jsons)] ->
    (match jsons with
     | `List [arg_label; core_type; class_type_json] ->
       let arg_label = Asttypes_deserializer.arg_label_of_yojson arg_label in
       let core_type = core_type_of_yojson core_type in
       let class_type = class_type_of_yojson class_type_json in
       Pcty_arrow (arg_label, core_type, class_type)
     | _ -> failwith "Invalid JSON format for Pcty_arrow")
  | `Assoc [("tag", `String "Pcty_extension"); ("contents", json)] -> Pcty_extension (extension_of_yojson json)
  | _ -> failwith "Invalid JSON format for class_type_desc"

and class_signature_of_yojson (json : Yojson.Safe.t) : class_signature =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassSignature" ->
      {
        pcsig_self = Yojson.Safe.Util.member "pcsigSelf" json |> core_type_of_yojson;
        pcsig_fields = Yojson.Safe.Util.member "pcsigFields" json |> (fun x -> match x with
          | `List x -> List.map class_type_field_of_yojson x
          | _ -> failwith "Invalid JSON format for pcsig_fields"
        );
      }
    | _ -> failwith "Invalid JSON format for class_signature: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_signature"


and class_type_field_of_yojson (json : Yojson.Safe.t) : class_type_field =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassTypeField" ->
      {
        pctf_desc = Yojson.Safe.Util.member "pctfDesc" json |> class_type_field_desc_of_yojson;
        pctf_loc = Yojson.Safe.Util.member "pctfLoc" json |> Location_deserializer.t_of_yojson;
        pctf_attributes = Yojson.Safe.Util.member "pctfAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for class_type_field: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_type_field"


and class_type_field_desc_of_yojson (json : Yojson.Safe.t) : class_type_field_desc =
  match json with
  | `Assoc [("tag", `String "PctfInherit"); ("contents", jsons)] -> Pctf_inherit (class_type_of_yojson jsons)
  | `Assoc [("tag", `String "PctfVal"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; mutable_flag; virtual_flag; core_type] ->
       let loc = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       let mutable_flag = Asttypes_deserializer.mutable_flag_of_yojson mutable_flag in
       let virtual_flag = Asttypes_deserializer.virtual_flag_of_yojson virtual_flag in
       let core_type = core_type_of_yojson core_type in
       Pctf_val (loc, mutable_flag, virtual_flag, core_type)
     | _ -> failwith "Invalid JSON format for Pctf_val")
  | `Assoc [("tag", `String "PctfMethod"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; private_flag; virtual_flag; core_type] ->
       let loc = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       let private_flag = Asttypes_deserializer.private_flag_of_yojson private_flag in
       let virtual_flag = Asttypes_deserializer.virtual_flag_of_yojson virtual_flag in
       let core_type = core_type_of_yojson core_type in
       Pctf_method (loc, private_flag, virtual_flag, core_type)
     | _ -> failwith "Invalid JSON format for Pctf_method")
  | `Assoc [("tag", `String "PctfConstraint"); ("contents", jsons)] ->
    (match jsons with
     | `List [core_type1; core_type2] ->
       let core_type1 = core_type_of_yojson core_type1 in
       let core_type2 = core_type_of_yojson core_type2 in
       Pctf_constraint (core_type1, core_type2)
     | _ -> failwith "Invalid JSON format for Pctf_constraint")
  | `Assoc [("tag", `String "PctfAttribute"); ("contents", json)] -> Pctf_attribute (attribute_of_yojson json)
  | `Assoc [("tag", `String "PctfExtension"); ("contents", json)] -> Pctf_extension (extension_of_yojson json)
  | _ -> failwith "Invalid JSON format for class_type_field_desc"

and class_infos_of_yojson (f : Yojson.Safe.t -> 'a) (json : Yojson.Safe.t) : 'a class_infos =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassInfos" ->
      {
        pci_virt = Yojson.Safe.Util.member "pciVirt" json |> Asttypes_deserializer.virtual_flag_of_yojson;
        pci_params = Yojson.Safe.Util.member "pciParams" json |> (fun x -> match x with
          | `List x_ -> List.map (fun y ->
            match y with
            | `List [t; v] -> (core_type_of_yojson t, Asttypes_deserializer.variance_of_yojson v)
            | _ -> failwith "Invalid JSON format for pci_params"
            ) x_
          | _ -> failwith "Invalid JSON format for pci_params"
        );
        pci_name = Yojson.Safe.Util.member "pciName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pci_expr = Yojson.Safe.Util.member "pciExpr" json |> f;
        pci_loc = Yojson.Safe.Util.member "pciLoc" json |> Location_deserializer.t_of_yojson;
        pci_attributes = Yojson.Safe.Util.member "pciAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for class_infos: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_infos"

and class_type_declaration_of_yojson (json : Yojson.Safe.t) : class_type_declaration = class_infos_of_yojson class_type_of_yojson json

and class_expr_of_yojson (json : Yojson.Safe.t) : class_expr =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassExpr" ->
      {
        pcl_desc = Yojson.Safe.Util.member "pclDesc" json |> class_expr_desc_of_yojson;
        pcl_loc = Yojson.Safe.Util.member "pclLoc" json |> Location_deserializer.t_of_yojson;
        pcl_attributes = Yojson.Safe.Util.member "pclAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for class_expr: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_expr"


and class_expr_desc_of_yojson (json : Yojson.Safe.t) : class_expr_desc =
  match json with
  | `Assoc [("tag", `String "PclConstr"); ("contents", jsons)] ->
    (match jsons with
     | `List [longident_loc_json; `List core_types_json] ->
       let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc_json in
       let core_types = List.map core_type_of_yojson core_types_json in
       Pcl_constr (longident_loc, core_types)
     | _ -> failwith "Invalid JSON format for Pcl_constr")
  | `Assoc [("tag", `String "PclStructure"); ("contents", json)] -> Pcl_structure (class_structure_of_yojson json)
  | `Assoc [("tag", `String "PclFun"); ("contents", jsons)] ->
    (match jsons with
     | `List [label; expression_json; pattern_json; class_expr_json] ->
       let label = Asttypes_deserializer.arg_label_of_yojson label in
       let expression = match expression_json with
        | `Null -> None
        | _ -> Some (expression_of_yojson expression_json) in
       let pattern = pattern_of_yojson pattern_json in
       let class_expr = class_expr_of_yojson class_expr_json in
       Pcl_fun (label, expression, pattern, class_expr)
     | _ -> failwith "Invalid JSON format for Pcl_fun")
  | `Assoc [("tag", `String "PclApply"); ("contents", jsons)] ->
    (match jsons with
     | `List [class_expr1_json; `List class_expr2_json] ->
       let class_expr1 = class_expr_of_yojson class_expr1_json in
       let class_expr2 = List.map (fun x -> 
        match x with
        | `List [l; e] -> (Asttypes_deserializer.arg_label_of_yojson l, expression_of_yojson e)
        | _ -> failwith "Invalid JSON format for Pcl_apply") class_expr2_json in
       Pcl_apply (class_expr1, class_expr2)
     | _ -> failwith "Invalid JSON format for Pcl_apply")
  | `Assoc [("tag", `String "PclLet"); ("contents", jsons)] ->
    (match jsons with
     | `List [rec_flag; `List value_binding_json; class_expr_json] ->
       let rec_flag = Asttypes_deserializer.rec_flag_of_yojson rec_flag in
       let value_binding = List.map value_binding_of_yojson value_binding_json in
       let class_expr = class_expr_of_yojson class_expr_json in
       Pcl_let (rec_flag, value_binding, class_expr)
     | _ -> failwith "Invalid JSON format for Pcl_let")
  | `Assoc [("tag", `String "PclConstraint"); ("contents", jsons)] ->
    (match jsons with
     | `List [class_expr1_json; class_type_json] ->
       let class_expr1 = class_expr_of_yojson class_expr1_json in
       let class_type = class_type_of_yojson class_type_json in
       Pcl_constraint (class_expr1, class_type)
     | _ -> failwith "Invalid JSON format for Pcl_constraint")
  | `Assoc [("tag", `String "PclExtension"); ("contents", json)] -> Pcl_extension (extension_of_yojson json)
  | `Assoc [("tag", `String "PclOpen"); ("contents", json)] -> 
    (match json with
      | `List [override_flag; longident_loc; class_expr] -> 
        let override_flag_type = Asttypes_deserializer.override_flag_of_yojson override_flag in
        let longident_loc_type = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc in
        let class_expr_type = class_expr_of_yojson class_expr in
        Pcl_open (override_flag_type, longident_loc_type, class_expr_type)
      | _ -> failwith "Invalid JSON format for Pcl_open")
  | _ -> failwith "Invalid JSON format for class_expr_desc"

and class_structure_of_yojson (json : Yojson.Safe.t) : class_structure =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassStructure" ->
      {
        pcstr_self = Yojson.Safe.Util.member "pcstrSelf" json |> pattern_of_yojson;
        pcstr_fields = Yojson.Safe.Util.member "pcstrFields" json |> (fun x -> match x with
          | `List x -> List.map class_field_of_yojson x
          | _ -> failwith "Invalid JSON format for pcstr_fields"
        );
      }
    | _ -> failwith "Invalid JSON format for class_structure: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_structure"


and class_field_of_yojson (json : Yojson.Safe.t) : class_field =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ClassField" ->
      {
        pcf_desc = Yojson.Safe.Util.member "pcfDesc" json |> class_field_desc_of_yojson;
        pcf_loc = Yojson.Safe.Util.member "pcfLoc" json |> Location_deserializer.t_of_yojson;
        pcf_attributes = Yojson.Safe.Util.member "pcfAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for class_field: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for class_field"

and class_field_desc_of_yojson (json : Yojson.Safe.t) : class_field_desc =
  match json with
  | `Assoc [("tag", `String "PcfInherit"); ("contents", _jsons)] -> Pcf_inherit ()
  | `Assoc [("tag", `String "PcfVal"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; mutable_flag; cf] ->
       let loc = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson loc in
       let mutable_flag = Asttypes_deserializer.mutable_flag_of_yojson mutable_flag in
       Pcf_val (loc, mutable_flag, class_field_kind_of_yojson cf)
     | _ -> failwith "Invalid JSON format for Pcf_val")
  | `Assoc [("tag", `String "PcfMethod"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; private_flag; cf] ->
       let loc = Asttypes_deserializer.loc_of_yojson Asttypes_deserializer.label_of_yojson loc in
       let private_flag = Asttypes_deserializer.private_flag_of_yojson private_flag in
       Pcf_method (loc, private_flag, class_field_kind_of_yojson cf)
     | _ -> failwith "Invalid JSON format for Pcf_method")
  | `Assoc [("tag", `String "PcfConstraint"); ("contents", jsons)] ->
    (match jsons with
     | `List [ct1; ct2] ->
       let ct1 = core_type_of_yojson ct1 in
       let ct2 = core_type_of_yojson ct2 in
       Pcf_constraint (ct1, ct2)
     | _ -> failwith "Invalid JSON format for Pcf_constraint")
  | `Assoc [("tag", `String "PcfInitializer"); ("contents", json)] -> Pcf_initializer (expression_of_yojson json)
  | `Assoc [("tag", `String "PcfAttribute"); ("contents", json)] -> Pcf_attribute (attribute_of_yojson json)
  | `Assoc [("tag", `String "PcfExtension"); ("contents", json)] -> Pcf_extension (extension_of_yojson json)
  | _ -> failwith "Invalid JSON format for class_field_desc"

and class_field_kind_of_yojson (jsons : Yojson.Safe.t) : class_field_kind = 
  match jsons with
  | `Assoc [("tag", `String "CfkVirtual"); ("contents", json)] -> Cfk_virtual (core_type_of_yojson json)
  | `Assoc [("tag", `String "CfkConcrete"); ("contents", jsons)] ->
    (match jsons with
     | `List [ovf; e] ->
       let ct1 = Asttypes_deserializer.override_flag_of_yojson ovf in
       let ct2 = expression_of_yojson e in
       Cfk_concrete (ct1, ct2)
     | _ -> failwith "Invalid JSON format for Pcf_constraint")
  | _ -> failwith "Invalid JSON format for class_field_kind"

and module_type_of_yojson (json : Yojson.Safe.t) : module_type = 
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ModuleType" ->
      {
        pmty_desc = Yojson.Safe.Util.member "pmtyDesc" json |> module_type_desc_of_yojson;
        pmty_loc = Yojson.Safe.Util.member "pmtyLoc" json |> Location_deserializer.t_of_yojson;
        pmty_attributes = Yojson.Safe.Util.member "pmtyAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON format for module_type: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON format for module_type"


and module_type_desc_of_yojson (jsons : Yojson.Safe.t) : module_type_desc = 
  match jsons with
  | `Assoc [("tag", `String "PmtyIdent"); ("contents", loc)] -> Pmty_ident ( Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson loc)
  | `Assoc [("tag", `String "PmtySignature"); ("contents", s)] -> Pmty_signature (signature_of_yojson s)
  | `Assoc [("tag", `String "PmtyFunctor"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; mt1; mt2] ->
       let loc_ = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       let mt1_ = match mt1 with
       | `Null -> None
       | _ -> Some (module_type_of_yojson mt1) in
       let mt2_ = module_type_of_yojson mt2 in
       Pmty_functor (loc_, mt1_, mt2_)
     | _ -> failwith "Invalid JSON format for PmtyFunctor")
  | `Assoc [("tag", `String "PmtyWith"); ("contents", jsons)] ->
    (match jsons with
     | `List [mt; `List l_list] ->
       let mt_ = module_type_of_yojson mt in
       let l_ = List.map with_constraint_of_yojson l_list in
       Pmty_with (mt_, l_)
     | _ -> failwith "Invalid JSON format for PmtyWith")
  | `Assoc [("tag", `String "PmtyTypeOf"); ("contents", jsons)] -> Pmty_typeof (module_expr_of_yojson jsons)
  | `Assoc [("tag", `String "PmtyExtension"); ("contents", jsons)] -> Pmty_extension (extension_of_yojson jsons)
  | `Assoc [("tag", `String "PmtyAlias"); ("contents", jsons)] -> Pmty_alias (Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson jsons)
  | _ -> failwith "Invalid JSON format for module_type_desc"

and signature_of_yojson (jsons : Yojson.Safe.t) : signature = 
  match jsons with
  | `List x -> List.map signature_item_of_yojson x
  | _ -> failwith "Invalid JSON for structure"

and signature_item_of_yojson (json : Yojson.Safe.t) : signature_item = 
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "SignatureItem" ->
      {
        psig_desc = Yojson.Safe.Util.member "psigDesc" json |> signature_item_desc_of_yojson;
        psig_loc = Yojson.Safe.Util.member "psigLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON for signature_item: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for signature_item"


and signature_item_desc_of_yojson (jsons : Yojson.Safe.t) : signature_item_desc =
  match jsons with
  | `Assoc [("tag", `String "PsigValue"); ("contents", jsons)] -> Psig_value (value_description_of_yojson jsons)
  | `Assoc [("tag", `String "PsigType"); ("contents", jsons)] ->
      (match jsons with
       | `List [mt; `List l_list] ->
          let mt_ = Asttypes_deserializer.rec_flag_of_yojson mt in
          let l_ = List.map type_declaration_of_yojson l_list in
          Psig_type (mt_, l_)
       | _ -> failwith "Invalid JSON format for PsigType")
  | `Assoc [("tag", `String "PsigTypExt"); ("contents", jsons)] -> Psig_typext (type_extension_of_yojson jsons)
  | `Assoc [("tag", `String "PsigException"); ("contents", jsons)] -> Psig_exception (extension_constructor_of_yojson jsons)
  | `Assoc [("tag", `String "PsigModule"); ("contents", jsons)] -> Psig_module (module_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PsigRecModule"); ("contents", `List jsons)] -> Psig_recmodule (List.map module_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PsigModType"); ("contents", jsons)] -> Psig_modtype (module_type_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PsigOpen"); ("contents", jsons)] -> Psig_open (open_description_of_yojson jsons)
  | `Assoc [("tag", `String "PsigInclude"); ("contents", jsons)] -> Psig_include (include_description_of_yojson jsons)
  | `Assoc [("tag", `String "PsigClass"); ("contents", _jsons)] -> Psig_class ()
  | `Assoc [("tag", `String "PsigClassType"); ("contents", `List jsons)] -> Psig_class_type (List.map class_type_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PsigAttribute"); ("contents", jsons)] -> Psig_attribute (attribute_of_yojson jsons)
  | `Assoc [("tag", `String "PsigExtension"); ("contents", jsons)] ->
    (match jsons with
     | `List [e; a] -> Psig_extension (extension_of_yojson e, attributes_of_yojson a)
     | _ -> failwith "Invalid JSON format for PsigExtension")
  | _ -> failwith "Invalid JSON for signature_item"


and module_declaration_of_yojson (json : Yojson.Safe.t) : module_declaration = 
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ModuleDeclaration" ->
      {
        pmd_name = Yojson.Safe.Util.member "pmdName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pmd_type = Yojson.Safe.Util.member "pmdType" json |> module_type_of_yojson;
        pmd_attributes = Yojson.Safe.Util.member "pmdAttributes" json |> attributes_of_yojson;
        pmd_loc = Yojson.Safe.Util.member "pmdLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON for module_declaration: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for module_declaration"


and module_type_declaration_of_yojson (json : Yojson.Safe.t) : module_type_declaration =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ModuleTypeDeclaration" ->
      {
        pmtd_name = Yojson.Safe.Util.member "pmtdName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pmtd_type = (match Yojson.Safe.Util.member "pmtdType" json with
                     | `Null -> None
                     | x -> Some (module_type_of_yojson x));
        pmtd_attributes = Yojson.Safe.Util.member "pmtdAttributes" json |> attributes_of_yojson;
        pmtd_loc = Yojson.Safe.Util.member "pmtdLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON for module_type_declaration: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for module_type_declaration"


and open_description_of_yojson (json : Yojson.Safe.t) : open_description = 
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "OpenDescription" ->
      {
        popen_lid = Yojson.Safe.Util.member "popenLid" json |> Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson;
        popen_override = Yojson.Safe.Util.member "popenOverride" json |> Asttypes_deserializer.override_flag_of_yojson;
        popen_loc = Yojson.Safe.Util.member "popenLoc" json |> Location_deserializer.t_of_yojson;
        popen_attributes = Yojson.Safe.Util.member "popenAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON for open_description: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for open_description"

(* TODO SPECIAL TYPE FUNCTION *)
and include_infos_of_yojson_module_type (f : Yojson.Safe.t -> module_type) (json : Yojson.Safe.t) : module_type include_infos =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "IncludeInfos" ->
      {
        pincl_mod = f (Yojson.Safe.Util.member "pinclMod" json);
        pincl_loc = Yojson.Safe.Util.member "pinclLoc" json |> Location_deserializer.t_of_yojson;
        pincl_attributes = Yojson.Safe.Util.member "pinclAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON for include_infos: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for include_infos"


and include_infos_of_yojson_module_exp (f : Yojson.Safe.t -> module_expr) (json : Yojson.Safe.t) : module_expr include_infos =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "IncludeInfos" ->
      {
        pincl_mod = f (Yojson.Safe.Util.member "pinclMod" json);
        pincl_loc = Yojson.Safe.Util.member "pinclLoc" json |> Location_deserializer.t_of_yojson;
        pincl_attributes = Yojson.Safe.Util.member "pinclAttributes" json |> attributes_of_yojson;
      }
    | _ -> failwith "Invalid JSON for include_infos: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for include_infos"

(* TODO SPCIAL TYPE FUNCTION *)


and include_description_of_yojson (jsons : Yojson.Safe.t) : include_description = 
  include_infos_of_yojson_module_type module_type_of_yojson jsons

and include_declaration_of_yojson (jsons :  Yojson.Safe.t) :include_declaration =
  include_infos_of_yojson_module_exp module_expr_of_yojson jsons
  
and with_constraint_of_yojson (jsons : Yojson.Safe.t) : with_constraint = 
  match jsons with
  | `Assoc [("tag", `String "PwithType"); ("contents", jsons)] -> 
    (match jsons with
    | `List [longident_loc; typ] -> 
      let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc in
      let typ = type_declaration_of_yojson typ in
      Pwith_type (longident_loc, typ)
    | _ -> failwith "Invalid JSON for with_constraint")
  | `Assoc [("tag", `String "PwithModule"); ("contents", jsons)] ->
    (match jsons with
    | `List [longident_loc; longident_loc2] -> 
      let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc in
      let longident_loc2 = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc2 in
      Pwith_module (longident_loc, longident_loc2)
    | _ -> failwith "Invalid JSON for with_constraint")
  | `Assoc [("tag", `String "PwithTypeSubst"); ("contents", jsons)] ->
    (match jsons with
    | `List [longident_loc; typ] -> 
      let longident_loc = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson longident_loc in
      let typ = type_declaration_of_yojson typ in
      Pwith_typesubst (longident_loc, typ)
    | _ -> failwith "Invalid JSON for with_constraint")
  | `Assoc [("tag", `String "PwithModSubst"); ("contents", jsons)] ->
    (match jsons with
    | `List [loc1; loc2] -> 
      let loc1_ = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson loc1 in
      let loc2_ = Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson loc2 in
      Pwith_modsubst (loc1_, loc2_)
    | _ -> failwith "Invalid JSON for with_constraint")
  | _ -> failwith "Invalid JSON for with_constraint"


  and module_expr_of_yojson (json : Yojson.Safe.t) : module_expr = 
    match json with
    | `Assoc _ -> 
      begin match Yojson.Safe.Util.member "tag" json with
      | `String "ModuleExpr" ->
        {
          pmod_desc = Yojson.Safe.Util.member "pmodDesc" json |> module_expr_desc_of_yojson;
          pmod_loc = Yojson.Safe.Util.member "pmodLoc" json |> Location_deserializer.t_of_yojson;
          pmod_attributes = Yojson.Safe.Util.member "pmodAttributes" json |> attributes_of_yojson;
        }
      | _ -> failwith "Invalid JSON for module_expr: missing or incorrect 'tag' field"
      end
    | _ -> failwith "Invalid JSON for module_expr"
  
and module_expr_desc_of_yojson (jsons : Yojson.Safe.t) : module_expr_desc =
  match jsons with
  | `Assoc [("tag", `String "PmodIdent"); ("contents", jsons)] -> Pmod_ident (Asttypes_deserializer.loc_of_yojson Longident_deserializer.t_of_yojson jsons)
  | `Assoc [("tag", `String "PmodStructure"); ("contents", jsons)] -> Pmod_structure (structure_of_yojson jsons)
  | `Assoc [("tag", `String "PmodFunctor"); ("contents", jsons)] ->
    (match jsons with
     | `List [loc; mt; me] ->
       let loc_ = Asttypes_deserializer.loc_of_yojson (fun x -> Yojson.Safe.Util.to_string x) loc in
       let mt_ = match mt with
        | `Null -> None
        | _ -> Some (module_type_of_yojson mt) in
       let me_ = module_expr_of_yojson me in
       Pmod_functor (loc_, mt_, me_)
     | _ -> failwith "Invalid JSON for module_expr_desc")
  | `Assoc [("tag", `String "PmodApply"); ("contents", jsons)] ->
    (match jsons with
     | `List [me1; me2] ->
       let me1_ = module_expr_of_yojson me1 in
       let me2_ = module_expr_of_yojson me2 in
       Pmod_apply (me1_, me2_)
     | _ -> failwith "Invalid JSON for module_expr_desc")
  | `Assoc [("tag", `String "PmodConstraint"); ("contents", jsons)] ->
    (match jsons with
     | `List [me; mt] ->
       let me_ = module_expr_of_yojson me in
       let mt_ = module_type_of_yojson mt in
       Pmod_constraint (me_, mt_)
     | _ -> failwith "Invalid JSON for module_expr_desc")
  | `Assoc [("tag", `String "PmodUnpack"); ("contents", jsons)] -> Pmod_unpack (expression_of_yojson jsons)
  | `Assoc [("tag", `String "PmodExtension"); ("contents", jsons)] -> Pmod_extension (extension_of_yojson jsons)
  | _ -> failwith "Invalid JSON for module_expr_desc"


and structure_of_yojson (jsons : Yojson.Safe.t) : structure = 
  match jsons with
  | `List x -> List.map structure_item_of_yojson x
  | _ -> failwith "Invalid JSON for structure"

and structure_item_of_yojson (json : Yojson.Safe.t) : structure_item =
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "StructureItem" ->
      {
        pstr_desc = Yojson.Safe.Util.member "pstrDesc" json |> structure_item_desc_of_yojson;
        pstr_loc = Yojson.Safe.Util.member "pstrLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON for structure_item: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for structure_item"


and structure_item_desc_of_yojson (jsons : Yojson.Safe.t) : structure_item_desc =
  match jsons with
  | `Assoc [("tag", `String "PstrEval"); ("contents", jsons)] -> 
    (match jsons with
    | `List [e ; a] -> Pstr_eval (expression_of_yojson e, attributes_of_yojson a)
    | _ -> failwith "Invalid JSON for structure_item_desc")
  | `Assoc [("tag", `String "PstrValue"); ("contents", jsons)] -> 
    (match jsons with
    | `List [rf; `List vbs] -> Pstr_value (Asttypes_deserializer.rec_flag_of_yojson rf, List.map value_binding_of_yojson vbs)
    | _ -> failwith "Invalid JSON for structure_item_desc")
  | `Assoc [("tag", `String "PstrPrimitive"); ("contents", jsons)] -> Pstr_primitive (value_description_of_yojson jsons)
  | `Assoc [("tag", `String "PstrType"); ("contents", jsons)] -> 
    (match jsons with
    | `List [rf; `List l] -> 
      let rf_ =  Asttypes_deserializer.rec_flag_of_yojson rf in
      let l_ = List.map type_declaration_of_yojson l in
      Pstr_type (rf_, l_)
    | _ -> failwith "Invalid JSON for structure_item_desc")
  | `Assoc [("tag", `String "PstrTypExt"); ("contents", jsons)] -> Pstr_typext (type_extension_of_yojson jsons)
  | `Assoc [("tag", `String "PstrException"); ("contents", jsons)] -> Pstr_exception (extension_constructor_of_yojson jsons)
  | `Assoc [("tag", `String "PstrModule"); ("contents", jsons)] -> Pstr_module (module_binding_of_yojson jsons)
  | `Assoc [("tag", `String "PstrRecModule"); ("contents", `List jsons)] -> Pstr_recmodule (List.map module_binding_of_yojson jsons)
  | `Assoc [("tag", `String "PstrModType"); ("contents", jsons)] -> Pstr_modtype (module_type_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PstrOpen"); ("contents", jsons)] -> Pstr_open (open_description_of_yojson jsons)
  | `Assoc [("tag", `String "PstrClass"); ("contents", _jsons)] -> Pstr_class ()
  | `Assoc [("tag", `String "PstrClassType"); ("contents", `List jsons)] -> Pstr_class_type (List.map class_type_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PstrInclude"); ("contents", jsons)] -> Pstr_include (include_declaration_of_yojson jsons)
  | `Assoc [("tag", `String "PstrAttribute"); ("contents", jsons)] -> Pstr_attribute (attribute_of_yojson jsons)
  | `Assoc [("tag", `String "PstrExtension"); ("contents", jsons)] -> 
    (match jsons with
    | `List [rf; l] -> 
      let rf_ =  extension_of_yojson rf in
      let l_ = attributes_of_yojson l in
      Pstr_extension (rf_, l_)
    | _ -> failwith "Invalid JSON for structure_item_desc")
  | _ -> failwith "Invalid JSON for structure_item_desc"

and value_binding_of_yojson (json : Yojson.Safe.t) : value_binding = 
  match json with
  | `Assoc _ -> 
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ValueBinding" ->
      {
        pvb_pat = Yojson.Safe.Util.member "pvbPat" json |> pattern_of_yojson;
        pvb_expr = Yojson.Safe.Util.member "pvbExpr" json |> expression_of_yojson;
        pvb_attributes = Yojson.Safe.Util.member "pvbAttributes" json |> attributes_of_yojson;
        pvb_loc = Yojson.Safe.Util.member "pvbLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON for value_binding: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for value_binding"


and module_binding_of_yojson (json : Yojson.Safe.t) : module_binding =
  match json with
  | `Assoc _ ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "ModuleBinding" ->
      {
        pmb_name = Yojson.Safe.Util.member "pmbName" json |> Asttypes_deserializer.loc_of_yojson Yojson.Safe.Util.to_string;
        pmb_expr = Yojson.Safe.Util.member "pmbExpr" json |> module_expr_of_yojson;
        pmb_attributes = Yojson.Safe.Util.member "pmbAttributes" json |> attributes_of_yojson;
        pmb_loc = Yojson.Safe.Util.member "pmbLoc" json |> Location_deserializer.t_of_yojson;
      }
    | _ -> failwith "Invalid JSON for module_binding: missing or incorrect 'tag' field"
    end
  | _ -> failwith "Invalid JSON for module_binding"
