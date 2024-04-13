open Parsetree

(* Type Constant *)

let constant_to_yojson (c : Parsetree.constant) : Yojson.Safe.t =
  match c with
  | Pconst_integer (value, suffix) ->
    `Assoc [
      ("tag", `String "PconstInteger");
      ("contents", `List [
        `String value;
        (match suffix with
        | None -> `Null
        | Some value -> `String (Char.escaped value))
      ])
    ]
  | Pconst_char value ->
    `Assoc [
        ("tag", `String "PconstChar");
        ("contents", `Int value)
    ]
  | Pconst_string (value, delim) ->
    `Assoc [
        ("tag", `String "PconstString");
        ("contents", `List [
              `String value;
              (match delim with
              | None -> `Null
              | Some value -> `String value)
        ])
    ]
  | Pconst_float (value, suffix) ->
    `Assoc [
        ("tag", `String "PconstFloat");
        ("contents", `List [
              `String value;
              (match suffix with
              | None -> `Null
              | Some value -> `String (Char.escaped value))
        ])
    ]

(* Type Attribute *)

let rec attribute_to_yojson (a : attribute) : Yojson.Safe.t =
  let (loc, payload) = a in
  `List [
    Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
    payload_to_yojson payload
  ]

(* Type Extension *)

and extension_to_yojson (e : extension) : Yojson.Safe.t =
  let (id, payload) = e in
  `List [
    Asttypes_serializer.loc_to_yojson (fun x -> `String x) id;
    payload_to_yojson payload
  ]

(* Type Attributes *)

and attributes_to_yojson (a : attributes) : Yojson.Safe.t =
  `List (List.map attribute_to_yojson a)

(* Type Payload *)

and payload_to_yojson (p : payload) : Yojson.Safe.t =
  match p with
  | PStr s -> `Assoc [
    ("tag", `String "PStr");
    ("contents", structure_to_yojson s)
  ]
  | PSig s -> `Assoc [
    ("tag", `String "PSig");
    ("contents", signature_to_yojson s)
  ]
  | PTyp t -> `Assoc [
    ("tag", `String "PTyp");
    ("contents", core_type_to_yojson t)
  ]
  | PPat (p, e) -> `Assoc [
    ("tag", `String "PPat");
    ("contents", `List [
      pattern_to_yojson p;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]

(* Type CoreType *)

and core_type_to_yojson (t : core_type) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "CoreType");
    ("ptypDesc", core_type_desc_to_yojson t.ptyp_desc);
    ("ptypLoc", Location_serializer.t_to_yojson t.ptyp_loc);
    ("ptypAttributes", attributes_to_yojson t.ptyp_attributes)
  ]

(* Type CoreTypeDesc *)

and core_type_desc_to_yojson (t : core_type_desc) : Yojson.Safe.t =
  match t with
  | Ptyp_any -> `Assoc [
    ("tag", `String "PtypAny")
  ]
  | Ptyp_var value -> `Assoc [
    ("tag", `String "PtypVar");
    ("contents", `String value)
  ]
  | Ptyp_arrow (label, t1, t2) -> `Assoc [
    ("tag", `String "PtypArrow");
    ("contents", `List [
      Asttypes_serializer.arg_label_to_yojson label;
      core_type_to_yojson t1;
      core_type_to_yojson t2
    ])
  ]
  | Ptyp_tuple t -> `Assoc [
    ("tag", `String "PtypTuple");
    ("contents", `List (List.map core_type_to_yojson t))
  ]
  | Ptyp_constr (loc, t) -> `Assoc [
    ("tag", `String "PtypConstr");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      `List (List.map core_type_to_yojson t)
    ])
  ]
  | Ptyp_object (o, cf) -> `Assoc [
    ("tag", `String "PtypObject");
    ("contents", `List [
      `List (List.map object_field_to_yojson o);
      Asttypes_serializer.closed_flag_to_yojson cf
    ])
  ]
  | Ptyp_class (loc, t) -> `Assoc [
    ("tag", `String "PtypClass");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      `List (List.map core_type_to_yojson t)
    ])
  ]
  | Ptyp_alias (t, value) -> `Assoc [
    ("tag", `String "PtypAlias");
    ("contents", `List [
      core_type_to_yojson t;
      `String value
    ])
  ]
  | Ptyp_variant (r, cf, l) ->
    `Assoc [
      ("tag", `String "PtypVariant");
      ("contents", `List [
        `List (List.map row_field_to_yojson r);
        Asttypes_serializer.closed_flag_to_yojson cf;
        (match l with
        | None -> `Null
        | Some value -> `List (List.map Asttypes_serializer.label_to_yojson value))]
      )
    ]
  | Ptyp_poly (l, t) -> `Assoc [
    ("tag", `String "PtypPoly");
    ("contents", `List [
      `List (List.map (Asttypes_serializer.loc_to_yojson (fun x -> `String x)) l);
      core_type_to_yojson t
    ])
   ]
  | Ptyp_package p -> `Assoc [
    ("tag", `String "PtypPackage");
    ("contents", package_type_to_yojson p)
   ]
  | Ptyp_extension e -> `Assoc [
    ("tag", `String "PtypExtension");
    ("contents", extension_to_yojson e)
   ]

(* Type PackageType *)

and package_type_to_yojson (p : package_type) : Yojson.Safe.t =
  `List [
    Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson (fst p);
    `List (
      List.map (
        fun (loc, t) -> `List [
          Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
          core_type_to_yojson t
      ]) (snd p)
     )
   ]

(* Type RowField *)

and row_field_to_yojson (r : row_field) : Yojson.Safe.t =
  match r with
  | Rtag (loc, attributes, boolean, core_type_list) -> `Assoc [
    ("tag", `String "Rtag");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      attributes_to_yojson attributes;
      `Bool boolean;
      `List (List.map core_type_to_yojson core_type_list)
    ])
   ]
  | Rinherit t -> `Assoc [
    ("tag", `String "Rinherit");
    ("contents", core_type_to_yojson t)
   ]

(* Type ObjectField *)

and object_field_to_yojson (o : object_field) : Yojson.Safe.t =
  match o with
  | Otag (loc, attributes, core_type) -> `Assoc [
    ("tag", `String "Otag");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      attributes_to_yojson attributes;
      core_type_to_yojson core_type
    ])
  ]
  | Oinherit core_type -> `Assoc [
    ("tag", `String "Oinherit");
    ("contents", core_type_to_yojson core_type)
  ]

(* Type Pattern *)

and pattern_to_yojson (p : pattern) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "Pattern");
    ("ppatDesc", pattern_desc_to_yojson p.ppat_desc);
    ("ppatLoc", Location_serializer.t_to_yojson p.ppat_loc);
    ("ppatAttributes", attributes_to_yojson p.ppat_attributes)
  ]

(* Type PatternDesc *)

and pattern_desc_to_yojson (p : pattern_desc) : Yojson.Safe.t =
  match p with
  | Ppat_any -> `Assoc [
    ("tag", `String "PpatAny")
  ]
  | Ppat_var value -> `Assoc [
    ("tag", `String "PpatVar");
    ("contents", Asttypes_serializer.loc_to_yojson (fun x -> `String x) value)
  ]
  | Ppat_alias (p, value) -> `Assoc [
    ("tag", `String "PpatAlias");
    ("contents", `List [
      pattern_to_yojson p;
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) value
    ])
  ]
  | Ppat_constant c -> `Assoc [
    ("tag", `String "PpatConstant");
    ("contents", constant_to_yojson c)
  ]
  | Ppat_interval (c1, c2) -> `Assoc [
    ("tag", `String "PpatInterval");
    ("contents", `List [
      constant_to_yojson c1;
      constant_to_yojson c2
    ])
  ]
  | Ppat_tuple p -> `Assoc [
    ("tag", `String "PpatTuple");
    ("contents", `List (List.map pattern_to_yojson p))
  ]
  | Ppat_construct (loc, p) -> `Assoc [
    ("tag", `String "PpatConstruct");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      (match p with
      | None -> `Null
      | Some value -> pattern_to_yojson value)
    ])
  ]
  | Ppat_variant (l, p) -> `Assoc [
    ("tag", `String "PpatVariant");
    ("contents", `List [
      Asttypes_serializer.label_to_yojson l;
      (match p with
      | None -> `Null
      | Some value -> pattern_to_yojson value)
    ])
  ]
  | Ppat_record (l, closed_flag) -> `Assoc [
    ("tag", `String "PpatRecord");
    ("contents", `List [
      `List (List.map (fun (loc, p) -> `List [
        Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
        pattern_to_yojson p
      ]) l);
      Asttypes_serializer.closed_flag_to_yojson closed_flag
    ])
  ]
  | Ppat_array p -> `Assoc [
    ("tag", `String "PpatArray");
    ("contents", `List (List.map pattern_to_yojson p))
  ]
  | Ppat_or (p1, p2) -> `Assoc [
    ("tag", `String "PpatOr");
    ("contents", `List [pattern_to_yojson p1; pattern_to_yojson p2])
  ]
  | Ppat_constraint (p, t) -> `Assoc [
    ("tag", `String "PpatConstraint");
    ("contents", `List [pattern_to_yojson p; core_type_to_yojson t])
  ]
  | Ppat_type l -> `Assoc [
    ("tag", `String "PpatType");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson l)
  ]
  | Ppat_lazy p -> `Assoc [
    ("tag", `String "PpatLazy");
    ("contents", pattern_to_yojson p)
  ]
  | Ppat_unpack value -> `Assoc [
    ("tag", `String "PpatUnpack");
    ("contents", Asttypes_serializer.loc_to_yojson (fun x -> `String x) value)
  ]
  | Ppat_exception p -> `Assoc [
    ("tag", `String "PpatException");
    ("contents", pattern_to_yojson p)
  ]
  | Ppat_extension e -> `Assoc [
    ("tag", `String "PpatExtension");
    ("contents", extension_to_yojson e)
  ]
  | Ppat_open (loc, p) -> `Assoc [
    ("tag", `String "PpatOpen");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      pattern_to_yojson p
    ])
  ]

(* Type Expression *)

and expression_to_yojson (e : expression) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "Expression");
    ("pexpDesc", expression_desc_to_yojson e.pexp_desc);
    ("pexpLoc", Location_serializer.t_to_yojson e.pexp_loc);
    ("pexpAttributes", attributes_to_yojson e.pexp_attributes)
  ]

(* Type ExpressionDesc *)

and expression_desc_to_yojson (e : expression_desc) : Yojson.Safe.t =
  match e with
  | Pexp_ident loc -> `Assoc [
    ("tag", `String "PexpIdent");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc)
  ]
  | Pexp_constant c -> `Assoc [
    ("tag", `String "PexpConstant");
    ("contents", constant_to_yojson c)
  ]
  | Pexp_let (rf, v, e) -> `Assoc [
    ("tag", `String "PexpLet");
    ("contents", `List [
      Asttypes_serializer.rec_flag_to_yojson rf;
      `List (List.map value_binding_to_yojson v);
      expression_to_yojson e
    ])
  ]
  | Pexp_function c -> `Assoc [
    ("tag", `String "PexpFunction");
    ("contents", `List (List.map case_to_yojson c))
  ]
  | Pexp_fun (l, d, p, e) -> `Assoc [
    ("tag", `String "PexpFun");
    ("contents", `List [
      Asttypes_serializer.arg_label_to_yojson l;
      (match d with
      | None -> `Null
      | Some value -> expression_to_yojson value);
      pattern_to_yojson p;
      expression_to_yojson e
    ])
  ]
  | Pexp_apply (e, l) -> `Assoc [
    ("tag", `String "PexpApply");
    ("contents", `List [
      expression_to_yojson e;
      `List (List.map (fun (l, e) -> `List [
        Asttypes_serializer.arg_label_to_yojson l;
        expression_to_yojson e
      ]) l)
    ])
  ]
  | Pexp_match (e, c) -> `Assoc [
    ("tag", `String "PexpMatch");
    ("contents", `List [
      expression_to_yojson e;
      `List (List.map case_to_yojson c)
    ])
  ]
  | Pexp_try (e, c) -> `Assoc [
    ("tag", `String "PexpTry");
    ("contents", `List [
      expression_to_yojson e;
      `List (List.map case_to_yojson c)
    ])
  ]
  | Pexp_tuple l -> `Assoc [
    ("tag", `String "PexpTuple");
    ("contents", `List (List.map expression_to_yojson l))
  ]
  | Pexp_construct (loc, e) -> `Assoc [
    ("tag", `String "PexpConstruct");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_variant (l, e) -> `Assoc [
    ("tag", `String "PexpVariant");
    ("contents", `List [
      Asttypes_serializer.label_to_yojson l;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_record (l, e) -> `Assoc [
    ("tag", `String "PexpRecord");
    ("contents", `List [
      `List (List.map (fun (loc, e) -> `List [
        Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
        expression_to_yojson e
      ]) l);
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_field (e, loc) -> `Assoc [
    ("tag", `String "PexpField");
    ("contents", `List [
      expression_to_yojson e;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc
    ])
  ]
  | Pexp_setfield (e1, loc, e2) -> `Assoc [
    ("tag", `String "PexpSetField");
    ("contents", `List [
      expression_to_yojson e1;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      expression_to_yojson e2
    ])
  ]
  | Pexp_array l -> `Assoc [
    ("tag", `String "PexpArray");
    ("contents", `List (List.map expression_to_yojson l))
  ]
  | Pexp_ifthenelse (e1, e2, e3) -> `Assoc [
    ("tag", `String "PexpIfThenElse");
    ("contents", `List [
      expression_to_yojson e1;
      expression_to_yojson e2;
      (match e3 with
      | None -> `Null
      | Some value -> expression_to_yojson value)
    ])
  ]
  | Pexp_sequence (e1, e2) -> `Assoc [
    ("tag", `String "PexpSequence");
    ("contents", `List [expression_to_yojson e1; expression_to_yojson e2])
  ]
  | Pexp_while (e1, e2) -> `Assoc [
    ("tag", `String "PexpWhile");
    ("contents", `List [expression_to_yojson e1; expression_to_yojson e2])
  ]
  | Pexp_for (p, e1, e2, d, e3) -> `Assoc [
    ("tag", `String "PexpFor");
    ("contents", `List [
      pattern_to_yojson p;
      expression_to_yojson e1;
      expression_to_yojson e2;
      Asttypes_serializer.direction_flag_to_yojson d;
      expression_to_yojson e3
    ])
  ]
  | Pexp_constraint (e, t) -> `Assoc [
    ("tag", `String "PexpConstraint");
    ("contents", `List [expression_to_yojson e; core_type_to_yojson t])
  ]
  | Pexp_coerce (e, t1, t2) -> `Assoc [
    ("tag", `String "PexpCoerce");
    ("contents", `List [
      expression_to_yojson e;
      (match t1 with None -> `Null | Some t -> core_type_to_yojson t);
      core_type_to_yojson t2
    ])
  ]
  | Pexp_send (e, l) -> `Assoc [
    ("tag", `String "PexpSend");
    ("contents", `List [
      expression_to_yojson e;
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson l
    ])
  ]
  | Pexp_new loc -> `Assoc [
    ("tag", `String "PexpNew");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc)
  ]
  | Pexp_setinstvar (loc, e) -> `Assoc [
    ("tag", `String "PexpSetinstvar");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      expression_to_yojson e
    ])
  ]
  | Pexp_override l -> `Assoc [
    ("tag", `String "PexpOverride");
    ("contents", `List [
      `List (List.map (fun (loc, e) -> `List [
        Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
        expression_to_yojson e
      ]) l)
    ])
  ]
  | Pexp_letmodule (loc, e1, e2) -> `Assoc [
    ("tag", `String "PexpLetmodule");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
      module_expr_to_yojson e1;
      expression_to_yojson e2
    ])
  ]
  | Pexp_letexception (ec, e) -> `Assoc [
    ("tag", `String "PexpLetexception");
    ("contents", `List [
      extension_constructor_to_yojson ec;
      expression_to_yojson e
    ])
  ]
  | Pexp_assert e -> `Assoc [
    ("tag", `String "PexpAssert");
    ("contents", expression_to_yojson e)
  ]
  | Pexp_lazy e -> `Assoc [
    ("tag", `String "PexpLazy");
    ("contents", expression_to_yojson e)
  ]
  | Pexp_poly (e, t) -> `Assoc [
    ("tag", `String "PexpPoly");
    ("contents", `List [
      expression_to_yojson e;
      (match t with None -> `Null | Some t -> core_type_to_yojson t)
    ])
  ]
  | Pexp_object o -> `Assoc [
    ("tag", `String "PexpObject");
    ("contents", class_structure_to_yojson o)
  ]
  | Pexp_newtype (loc, e) -> `Assoc [
    ("tag", `String "PexpNewtype");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
      expression_to_yojson e
    ])
  ]
  | Pexp_pack me -> `Assoc [
    ("tag", `String "PexpPack");
    ("contents", module_expr_to_yojson me)
  ]
  | Pexp_open (ovf ,loc, e) -> `Assoc [
    ("tag", `String "PexpOpen");
    ("contents", `List [
      Asttypes_serializer.override_flag_to_yojson ovf;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      expression_to_yojson e
    ])
  ]
  | Pexp_extension e -> `Assoc [
    ("tag", `String "PexpExtension");
    ("contents", extension_to_yojson e)
  ]
  | Pexp_unreachable -> `Assoc [
    ("tag", `String "PexpUnreachable")
  ]

(* Type Case *)

and case_to_yojson (c : case) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "Case");
    ("pcLhs", pattern_to_yojson c.pc_lhs);
    ("pcGuard", (match c.pc_guard with
    | None -> `Null
    | Some value -> expression_to_yojson value));
    ("pcRhs", expression_to_yojson c.pc_rhs)
  ]

(* Type ValueDescription *)

and value_description_to_yojson (v : value_description) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ValueDescription");
    ("pvalName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) v.pval_name);
    ("pvalType", core_type_to_yojson v.pval_type);
    ("pvalPrim", `List (List.map (fun x -> `String x) v.pval_prim));
    ("pvalAttributes", attributes_to_yojson v.pval_attributes);
    ("pvalLoc", Location_serializer.t_to_yojson v.pval_loc)
  ]

(* Type TypeDeclaration *)

and type_declaration_to_yojson (t : type_declaration) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "TypeDeclaration");
    ("ptypeName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) t.ptype_name);
    ("ptypeParams", `List (List.map (fun (t, v) ->
      `List
      [core_type_to_yojson t;
      Asttypes_serializer.variance_to_yojson v])
      t.ptype_params));
    ("ptypeCstrs", `List (List.map (fun (t1, t2, l) ->
      `List
      [
        core_type_to_yojson t1;
        core_type_to_yojson t2;
        Location_serializer.t_to_yojson l
      ])
      t.ptype_cstrs));
    ("ptypeKind", type_kind_to_yojson t.ptype_kind);
    ("ptypePrivate", Asttypes_serializer.private_flag_to_yojson t.ptype_private);
    ("ptypeManifest", (match t.ptype_manifest with
    | None -> `Null
    | Some value -> core_type_to_yojson value));
    ("ptypeAttributes", attributes_to_yojson t.ptype_attributes);
    ("ptypeLoc", Location_serializer.t_to_yojson t.ptype_loc)
  ]

(* Type TypeKind *)

and type_kind_to_yojson (t : type_kind) : Yojson.Safe.t =
  match t with
  | Ptype_abstract -> `Assoc [
    ("tag", `String "PtypeAbstract")
  ]
  | Ptype_variant l -> `Assoc [
    ("tag", `String "PtypeVariant");
    ("contents", `List (List.map constructor_declaration_to_yojson l))
  ]
  | Ptype_record l -> `Assoc [
    ("tag", `String "PtypeRecord");
    ("contents", `List (List.map label_declaration_to_yojson l))
  ]
  | Ptype_open -> `Assoc [
    ("tag", `String "PtypeOpen")
  ]

(* Type Label *)

and label_declaration_to_yojson (l : label_declaration) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "LabelDeclaration");
    ("pldName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) l.pld_name);
    ("pldMutable", Asttypes_serializer.mutable_flag_to_yojson l.pld_mutable);
    ("pldType", core_type_to_yojson l.pld_type);
    ("pldLoc", Location_serializer.t_to_yojson l.pld_loc);
    ("pldAttributes", attributes_to_yojson l.pld_attributes)
  ]

(* Type ConstructorDeclaration *)

and constructor_declaration_to_yojson (c : constructor_declaration) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ConstructorDeclaration");
    ("pcdName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) c.pcd_name);
    ("pcdArgs", constructor_arguments_to_yojson c.pcd_args);
    ("pcdRes", (match c.pcd_res with
    | None -> `Null
    | Some value -> core_type_to_yojson value));
    ("pcdLoc", Location_serializer.t_to_yojson c.pcd_loc);
    ("pcdAttributes", attributes_to_yojson c.pcd_attributes)
  ]

(* Type ConstructorArguments *)

and constructor_arguments_to_yojson (c : constructor_arguments) : Yojson.Safe.t =
  match c with
  | Pcstr_tuple l -> `Assoc [
    ("tag", `String "PcstrTuple");
    ("contents", `List (List.map core_type_to_yojson l))
  ]
  | Pcstr_record l -> `Assoc [
    ("tag", `String "PcstrRecord");
    ("contents", `List (List.map label_declaration_to_yojson l))
  ]

(* Type TypeExtension *)

and type_extension_to_yojson (t : type_extension) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "TypeExtension");
    ("ptyextPath", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson t.ptyext_path);
    ("ptyextParams", `List (List.map (fun (t, v) ->
      `List
      [ core_type_to_yojson t;
        Asttypes_serializer.variance_to_yojson v
      ])
      t.ptyext_params));
    ("ptyextConstructors", `List (List.map extension_constructor_to_yojson t.ptyext_constructors));
    ("ptyextPrivate", Asttypes_serializer.private_flag_to_yojson t.ptyext_private);
    ("ptyextAttributes", attributes_to_yojson t.ptyext_attributes)
  ]

(* Type ExtensionConstructor *)

and extension_constructor_to_yojson (e : extension_constructor) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ExtensionConstructor");
    ("pextName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) e.pext_name);
    ("pextKind", extension_constructor_kind_to_yojson e.pext_kind);
    ("pextLoc", Location_serializer.t_to_yojson e.pext_loc);
    ("pextAttributes", attributes_to_yojson e.pext_attributes)
  ]

(* Type ExtensionConstructorKind *)

and extension_constructor_kind_to_yojson (e : extension_constructor_kind) : Yojson.Safe.t =
  match e with
  | Pext_decl (ca, ct) -> `Assoc [
    ("tag", `String "PextDecl");
    ("contents", `List [
      constructor_arguments_to_yojson ca;
      (match ct with
      | None -> `Null
      | Some value -> core_type_to_yojson value)
    ])
  ]
  | Pext_rebind loc -> `Assoc [
    ("tag", `String "PextRebind");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc)
  ]

(* Type ClassType *)

and class_type_to_yojson (c : class_type) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassType");
    ("pctyDesc", class_type_desc_to_yojson c.pcty_desc);
    ("pctyLoc", Location_serializer.t_to_yojson c.pcty_loc);
    ("pctyAttributes", attributes_to_yojson c.pcty_attributes)
  ]

(* Type ClassTypeDesc *)

and class_type_desc_to_yojson (c : class_type_desc) : Yojson.Safe.t =
  match c with
  | Pcty_constr (loc, l) -> `Assoc [
    ("tag", `String "PctyConstr");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      `List (List.map core_type_to_yojson l)
    ])
  ]
  | Pcty_signature l -> `Assoc [
    ("tag", `String "PctySignature");
    ("contents", class_signature_to_yojson l)
  ]
  | Pcty_arrow (l, c1, c2) -> `Assoc [
    ("tag", `String "PctyArrow");
    ("contents", `List [
      Asttypes_serializer.arg_label_to_yojson l;
      core_type_to_yojson c1;
      class_type_to_yojson c2
    ])
  ]
  | Pcty_extension e -> `Assoc [
    ("tag", `String "PctyExtension");
    ("contents", extension_to_yojson e)
  ]
  | Pcty_open (ovf, loc, c) -> `Assoc [
    ("tag", `String "PctyOpen");
    ("contents", `List [
      Asttypes_serializer.override_flag_to_yojson ovf;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      class_type_to_yojson c
    ])
  ]

(* Type ClassSignature *)

and class_signature_to_yojson (c : class_signature) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassSignature");
    ("pcsigSelf", core_type_to_yojson c.pcsig_self);
    ("pcsigFields", `List (List.map class_type_field_to_yojson c.pcsig_fields))
  ]

(* Type ClassTypeField *)

and class_type_field_to_yojson (c : class_type_field) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassTypeField");
    ("pctfDesc", class_type_field_desc_to_yojson c.pctf_desc);
    ("pctfLoc", Location_serializer.t_to_yojson c.pctf_loc);
    ("pctfAttributes", attributes_to_yojson c.pctf_attributes)
  ]

(* Type ClassTypeFieldDesc *)

and class_type_field_desc_to_yojson (c : class_type_field_desc) : Yojson.Safe.t =
  match c with
  | Pctf_inherit c -> `Assoc [
    ("tag", `String "PctfInherit");
    ("contents", class_type_to_yojson c)
  ]
  | Pctf_val (loc, m, v, c) -> `Assoc [
    ("tag", `String "PctfVal");
    ("contents", `List [`List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      Asttypes_serializer.mutable_flag_to_yojson m;
      Asttypes_serializer.virtual_flag_to_yojson v;
      core_type_to_yojson c
    ]])
  ]
  | Pctf_method (loc, p, v, c) -> `Assoc [
    ("tag", `String "PctfMethod");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      Asttypes_serializer.private_flag_to_yojson p;
      Asttypes_serializer.virtual_flag_to_yojson v;
      core_type_to_yojson c
    ])
  ]
  | Pctf_constraint (c1, c2) -> `Assoc [
    ("tag", `String "PctfConstraint");
    ("contents", `List [core_type_to_yojson c1; core_type_to_yojson c2])
  ]
  | Pctf_attribute a -> `Assoc [
    ("tag", `String "PctfAttribute");
    ("contents", attribute_to_yojson a)
  ]
  | Pctf_extension e -> `Assoc [
    ("tag", `String "PctfExtension");
    ("contents", extension_to_yojson e)
  ]

(* Type ClassInfos *)

and class_infos_to_yojson (f : 'a -> Yojson.Safe.t) (c : 'a class_infos) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassInfos");
    ("pciVirt", Asttypes_serializer.virtual_flag_to_yojson c.pci_virt);
    ("pciParams", `List (List.map (fun (t, v) ->
      `List
      [core_type_to_yojson t;
      Asttypes_serializer.variance_to_yojson v])
      c.pci_params));
    ("pciName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) c.pci_name);
    ("pciExpr", f c.pci_expr);
    ("pciLoc", Location_serializer.t_to_yojson c.pci_loc);
    ("pciAttributes", attributes_to_yojson c.pci_attributes)
  ]

(* Type ClassTypeDeclaration *)

and class_type_declaration_to_yojson (c : class_type_declaration) : Yojson.Safe.t =
    class_infos_to_yojson class_type_to_yojson c

(* Type ClassExpression *)

and class_expr_to_yojson (c : class_expr) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassExpr");
    ("pclDesc", class_expr_desc_to_yojson c.pcl_desc);
    ("pclLoc", Location_serializer.t_to_yojson c.pcl_loc);
    ("pclAttributes", attributes_to_yojson c.pcl_attributes)
  ]

(* Type ClassExpressionDesc *)

and class_expr_desc_to_yojson (c : class_expr_desc) : Yojson.Safe.t =
  match c with
  | Pcl_constr (loc, l) -> `Assoc [
    ("tag", `String "PclConstr");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      `List (List.map core_type_to_yojson l)
    ])
  ]
  | Pcl_structure c -> `Assoc [
    ("tag", `String "PclStructure");
    ("contents", class_structure_to_yojson c)
  ]
  | Pcl_fun (l, e, p, c) -> `Assoc [
    ("tag", `String "PclFun");
    ("contents", `List [
      Asttypes_serializer.arg_label_to_yojson l;
      (match e with
      | None -> `Null
      | Some value -> expression_to_yojson value);
      pattern_to_yojson p;
      class_expr_to_yojson c
    ])
  ]
  | Pcl_apply (c, l) -> `Assoc [
    ("tag", `String "PclApply");
    ("contents", `List [
      class_expr_to_yojson c;
      `List (List.map (fun (l, e) -> `List [
        Asttypes_serializer.arg_label_to_yojson l;
        expression_to_yojson e
      ]) l)
    ])
  ]
  | Pcl_let (rf, v, c) -> `Assoc [
    ("tag", `String "PclLet");
    ("contents", `List [
      Asttypes_serializer.rec_flag_to_yojson rf;
      `List (List.map value_binding_to_yojson v);
      class_expr_to_yojson c
    ])
  ]
  | Pcl_constraint (c, t) -> `Assoc [
    ("tag", `String "PclConstraint");
    ("contents", `List [class_expr_to_yojson c; class_type_to_yojson t])
  ]
  | Pcl_extension e -> `Assoc [
    ("tag", `String "PclExtension");
    ("contents", extension_to_yojson e)
  ]
  | Pcl_open (ovf, loc, c) -> `Assoc [
    ("tag", `String "PclOpen");
    ("contents", `List [
      Asttypes_serializer.override_flag_to_yojson ovf;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      class_expr_to_yojson c
    ])
  ]

(* Type ClassStructure *)

and class_structure_to_yojson (c : class_structure) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassStructure");
    ("pcstrSelf", pattern_to_yojson c.pcstr_self);
    ("pcstrFields", `List (List.map class_field_to_yojson c.pcstr_fields))
  ]

(* Type ClassField *)

and class_field_to_yojson (c : class_field) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ClassField");
    ("pcfDesc", class_field_desc_to_yojson c.pcf_desc);
    ("pcfLoc", Location_serializer.t_to_yojson c.pcf_loc);
    ("pcfAttributes", attributes_to_yojson c.pcf_attributes)
  ]

(* Type ClassFieldDesc *)

and class_field_desc_to_yojson (c : class_field_desc) : Yojson.Safe.t =
  match c with
  | Pcf_inherit _u -> `Assoc [
    ("tag", `String "PcfInherit");
    ("contents", `List [])
  ]
  | Pcf_val (loc, m, cfk) -> `Assoc [
    ("tag", `String "PcfVal");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      Asttypes_serializer.mutable_flag_to_yojson m;
      class_field_kind_to_yojson cfk
    ])
  ]
  | Pcf_method (loc, p, cfk) -> `Assoc [
    ("tag", `String "PcfMethod");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Asttypes_serializer.label_to_yojson loc;
      Asttypes_serializer.private_flag_to_yojson p;
      class_field_kind_to_yojson cfk
    ])
  ]
  | Pcf_constraint (c1, c2) -> `Assoc [
    ("tag", `String "PcfConstraint");
    ("contents", `List [core_type_to_yojson c1; core_type_to_yojson c2])
  ]
  | Pcf_initializer e -> `Assoc [
    ("tag", `String "PcfInitializer");
    ("contents", expression_to_yojson e)
  ]
  | Pcf_attribute a -> `Assoc [
    ("tag", `String "PcfAttribute");
    ("contents", attribute_to_yojson a)
  ]
  | Pcf_extension e -> `Assoc [
    ("tag", `String "PcfExtension");
    ("contents", extension_to_yojson e)
  ]

(* Type ClassFieldKind *)

and class_field_kind_to_yojson (c : class_field_kind) : Yojson.Safe.t =
  match c with
  | Cfk_virtual t -> `Assoc [
    ("tag", `String "CfkVirtual");
    ("contents", core_type_to_yojson t)
  ]
  | Cfk_concrete (ovf, e) -> `Assoc [
    ("tag", `String "CfkConcrete");
    ("contents", `List [
      Asttypes_serializer.override_flag_to_yojson ovf;
      expression_to_yojson e
    ])
  ]

(* Type ModuleType *)

and module_type_to_yojson (m : module_type) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ModuleType");
    ("pmtyDesc", module_type_desc_to_yojson m.pmty_desc);
    ("pmtyLoc", Location_serializer.t_to_yojson m.pmty_loc);
    ("pmtyAttributes", attributes_to_yojson m.pmty_attributes)
  ]

(* Type ModuleTypeDesc *)

and module_type_desc_to_yojson (m : module_type_desc) : Yojson.Safe.t =
  match m with
  | Pmty_ident loc -> `Assoc [
    ("tag", `String "PmtyIdent");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc)
  ]
  | Pmty_signature s -> `Assoc [
    ("tag", `String "PmtySignature");
    ("contents", signature_to_yojson s)
  ]
  | Pmty_functor (loc, mt1, mt2) -> `Assoc [
    ("tag", `String "PmtyFunctor");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
      (match mt1 with None -> `Null | Some value -> module_type_to_yojson value);
      module_type_to_yojson mt2
    ])
  ]
  | Pmty_with (mt, l) -> `Assoc [
    ("tag", `String "PmtyWith");
    ("contents", `List [
      module_type_to_yojson mt;
      `List (List.map with_constraint_to_yojson l)
    ])
  ]
  | Pmty_typeof me -> `Assoc [
    ("tag", `String "PmtyTypeOf");
    ("contents", module_expr_to_yojson me)
  ]
  | Pmty_extension e -> `Assoc [
    ("tag", `String "PmtyExtension");
    ("contents", extension_to_yojson e)
  ]
  | Pmty_alias loc -> `Assoc [
    ("tag", `String "PmtyAlias");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc)
  ]

(* Type Signature *)

and signature_to_yojson (s : signature) : Yojson.Safe.t =
  `List (List.map signature_item_to_yojson s)

(* Type SignatureItem *)

and signature_item_to_yojson (s : signature_item) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "SignatureItem");
    ("psigDesc", signature_item_desc_to_yojson s.psig_desc);
    ("psigLoc", Location_serializer.t_to_yojson s.psig_loc)
  ]

(* Type SignatureItemDesc *)

and signature_item_desc_to_yojson (s : signature_item_desc) : Yojson.Safe.t =
  match s with
  | Psig_value v -> `Assoc [
    ("tag", `String "PsigValue");
    ("contents", value_description_to_yojson v)
  ]
  | Psig_type (rf, l) -> `Assoc [
    ("tag", `String "PsigType");
    ("contents", `List [
      Asttypes_serializer.rec_flag_to_yojson rf;
      `List (List.map type_declaration_to_yojson l)
    ])
  ]
  | Psig_typext te -> `Assoc [
    ("tag", `String "PsigTypExt");
    ("contents", type_extension_to_yojson te)
  ]
  | Psig_exception e -> `Assoc [
    ("tag", `String "PsigException");
    ("contents", extension_constructor_to_yojson e)
  ]
  | Psig_module m -> `Assoc [
    ("tag", `String "PsigModule");
    ("contents", module_declaration_to_yojson m)
  ]
  | Psig_recmodule l -> `Assoc [
    ("tag", `String "PsigRecModule");
    ("contents", `List (List.map module_declaration_to_yojson l))
  ]
  | Psig_modtype m -> `Assoc [
    ("tag", `String "PsigModType");
    ("contents", module_type_declaration_to_yojson m)
  ]
  | Psig_open od -> `Assoc [
    ("tag", `String "PsigOpen");
    ("contents", open_description_to_yojson od)
  ]
  | Psig_include id -> `Assoc [
    ("tag", `String "PsigInclude");
    ("contents", include_description_to_yojson id)
  ]
  | Psig_class _u -> `Assoc [
    ("tag", `String "PsigClass");
    ("contents", `List [])
  ]
  | Psig_class_type l -> `Assoc [
    ("tag", `String "PsigClassType");
    ("contents", `List (List.map class_type_declaration_to_yojson l))
  ]
  | Psig_attribute a -> `Assoc [
    ("tag", `String "PsigAttribute");
    ("contents", attribute_to_yojson a)
  ]
  | Psig_extension (e, a) -> `Assoc [
    ("tag", `String "PsigExtension");
    ("contents", `List [
      extension_to_yojson e;
      attributes_to_yojson a
    ])
  ]

(* Type ModuleDeclaration *)

and module_declaration_to_yojson (m : module_declaration) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ModuleDeclaration");
    ("pmdName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) m.pmd_name);
    ("pmdType", module_type_to_yojson m.pmd_type);
    ("pmdAttributes", attributes_to_yojson m.pmd_attributes);
    ("pmdLoc", Location_serializer.t_to_yojson m.pmd_loc)
  ]

(* Type ModuleTypeDeclaration *)

and module_type_declaration_to_yojson (m : module_type_declaration) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ModuleTypeDeclaration");
    ("pmtdName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) m.pmtd_name);
    ("pmtdType", (match m.pmtd_type with
    | None -> `Null
    | Some value -> module_type_to_yojson value));
    ("pmtdAttributes", attributes_to_yojson m.pmtd_attributes);
    ("pmtdLoc", Location_serializer.t_to_yojson m.pmtd_loc)
  ]

(* Type OpenDescription *)

and open_description_to_yojson (o : open_description) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "OpenDescription");
    ("popenLid", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson o.popen_lid);
    ("popenOverride", Asttypes_serializer.override_flag_to_yojson o.popen_override);
    ("popenLoc", Location_serializer.t_to_yojson o.popen_loc);
    ("popenAttributes", attributes_to_yojson o.popen_attributes)
  ]

(* Type IncludeInfos *)

and include_infos_to_yojson_module_type (f : module_type -> Yojson.Safe.t) (i : module_type include_infos) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "IncludeInfos");
    ("pinclMod", f i.pincl_mod);
    ("pinclLoc", Location_serializer.t_to_yojson i.pincl_loc);
    ("pinclAttributes", attributes_to_yojson i.pincl_attributes)
  ]

and include_infos_to_yojson_module_expr (f : module_expr -> Yojson.Safe.t) (i : module_expr include_infos) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "IncludeInfos");
    ("pinclMod", f i.pincl_mod);
    ("pinclLoc", Location_serializer.t_to_yojson i.pincl_loc);
    ("pinclAttributes", attributes_to_yojson i.pincl_attributes)
  ]

(* Type IncludeDescription *)

and include_description_to_yojson (i : include_description) : Yojson.Safe.t =
  include_infos_to_yojson_module_type module_type_to_yojson i

(* Type IncludeDeclaration *)

and include_declaration_to_yojson (i : include_declaration) : Yojson.Safe.t =
  include_infos_to_yojson_module_expr module_expr_to_yojson i

(* Type WithConstraint *)

and with_constraint_to_yojson (w : with_constraint) : Yojson.Safe.t =
  match w with
  | Pwith_type (loc, td) -> `Assoc [
    ("tag", `String "PwithType");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      type_declaration_to_yojson td
    ])
  ]
  | Pwith_module (loc1, loc2) -> `Assoc [
    ("tag", `String "PwithModule");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc1;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc2
    ])
  ]
  | Pwith_typesubst (loc, td) -> `Assoc [
    ("tag", `String "PwithTypeSubst");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc;
      type_declaration_to_yojson td
    ])
  ]
  | Pwith_modsubst (loc1, loc2) -> `Assoc [
    ("tag", `String "PwithModSubst");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc1;
      Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc2
    ])
  ]

(* Type ModuleExpr *)

and module_expr_to_yojson (m : module_expr) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ModuleExpr");
    ("pmodDesc", module_expr_desc_to_yojson m.pmod_desc);
    ("pmodLoc", Location_serializer.t_to_yojson m.pmod_loc);
    ("pmodAttributes", attributes_to_yojson m.pmod_attributes)
  ]

(* Type ModuleExprDesc *)

and module_expr_desc_to_yojson (m : module_expr_desc) : Yojson.Safe.t =
  match m with
  | Pmod_ident loc -> `Assoc [
    ("tag", `String "PmodIdent");
    ("contents", Asttypes_serializer.loc_to_yojson Longident_serializer.t_to_yojson loc)
  ]
  | Pmod_structure s -> `Assoc [
    ("tag", `String "PmodStructure");
    ("contents", structure_to_yojson s)
  ]
  | Pmod_functor (loc, mt, me) -> `Assoc [
    ("tag", `String "PmodFunctor");
    ("contents", `List [
      Asttypes_serializer.loc_to_yojson (fun x -> `String x) loc;
      (match mt with None -> `Null | Some value -> module_type_to_yojson value);
      module_expr_to_yojson me
    ])
  ]
  | Pmod_apply (me1, me2) -> `Assoc [
    ("tag", `String "PmodApply");
    ("contents", `List [module_expr_to_yojson me1; module_expr_to_yojson me2])
  ]
  | Pmod_constraint (me, mt) -> `Assoc [
    ("tag", `String "PmodConstraint");
    ("contents", `List [module_expr_to_yojson me; module_type_to_yojson mt])
  ]
  | Pmod_unpack e -> `Assoc [
    ("tag", `String "PmodUnpack");
    ("contents", expression_to_yojson e)
  ]
  | Pmod_extension e -> `Assoc [
    ("tag", `String "PmodExtension");
    ("contents", extension_to_yojson e)
  ]

(* Type Structure *)

and structure_to_yojson (s : structure) : Yojson.Safe.t =
  `List (List.map structure_item_to_yojson s)

(* Type StructureItem *)

and structure_item_to_yojson (s : structure_item) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "StructureItem");
    ("pstrDesc", structure_item_desc_to_yojson s.pstr_desc);
    ("pstrLoc", Location_serializer.t_to_yojson s.pstr_loc)
  ]

(* Type StructureItemDesc *)

and structure_item_desc_to_yojson (s : structure_item_desc) : Yojson.Safe.t =
  match s with
  | Pstr_eval (e, a) -> `Assoc [
    ("tag", `String "PstrEval");
    ("contents", `List [expression_to_yojson e; attributes_to_yojson a])
  ]
  | Pstr_value (rf, l) -> `Assoc [
    ("tag", `String "PstrValue");
    ("contents", `List [
      Asttypes_serializer.rec_flag_to_yojson rf;
      `List (List.map value_binding_to_yojson l)
    ])
  ]
  | Pstr_primitive v -> `Assoc [
    ("tag", `String "PstrPrimitive");
    ("contents", value_description_to_yojson v)
  ]
  | Pstr_type (rf, l) -> `Assoc [
    ("tag", `String "PstrType");
    ("contents", `List [
      Asttypes_serializer.rec_flag_to_yojson rf;
      `List (List.map type_declaration_to_yojson l)
    ])
  ]
  | Pstr_typext te -> `Assoc [
    ("tag", `String "PstrTypExt");
    ("contents", type_extension_to_yojson te)
  ]
  | Pstr_exception e -> `Assoc [
    ("tag", `String "PstrException");
    ("contents", extension_constructor_to_yojson e)
  ]
  | Pstr_module m -> `Assoc [
    ("tag", `String "PstrModule");
    ("contents", module_binding_to_yojson m)
  ]
  | Pstr_recmodule l -> `Assoc [
    ("tag", `String "PstrRecModule");
    ("contents", `List (List.map module_binding_to_yojson l))
  ]
  | Pstr_modtype m -> `Assoc [
    ("tag", `String "PstrModType");
    ("contents", module_type_declaration_to_yojson m)
  ]
  | Pstr_open o -> `Assoc [
    ("tag", `String "PstrOpen");
    ("contents", open_description_to_yojson o)
  ]
  | Pstr_class _u -> `Assoc [
    ("tag", `String "PstrClass");
    ("contents", `List [])
  ]
  | Pstr_class_type l -> `Assoc [
    ("tag", `String "PstrClassType");
    ("contents", `List (List.map class_type_declaration_to_yojson l))
  ]
  | Pstr_include id -> `Assoc [
    ("tag", `String "PstrInclude");
    ("contents", include_declaration_to_yojson id)
  ]
  | Pstr_attribute a -> `Assoc [
    ("tag", `String "PstrAttribute");
    ("contents", attribute_to_yojson a)
  ]
  | Pstr_extension (e, a) -> `Assoc [
    ("tag", `String "PstrExtension");
    ("contents", `List [
      extension_to_yojson e;
      attributes_to_yojson a
    ])
  ]

(* Type ValueBinding *)

and value_binding_to_yojson (v : value_binding) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ValueBinding");
    ("pvbPat", pattern_to_yojson v.pvb_pat);
    ("pvbExpr", expression_to_yojson v.pvb_expr);
    ("pvbAttributes", attributes_to_yojson v.pvb_attributes);
    ("pvbLoc", Location_serializer.t_to_yojson v.pvb_loc)
  ]

(* Type ModuleBinding *)

and module_binding_to_yojson (m : module_binding) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "ModuleBinding");
    ("pmbName", Asttypes_serializer.loc_to_yojson (fun x -> `String x) m.pmb_name);
    ("pmbExpr", module_expr_to_yojson m.pmb_expr);
    ("pmbAttributes", attributes_to_yojson m.pmb_attributes);
    ("pmbLoc", Location_serializer.t_to_yojson m.pmb_loc)
  ]