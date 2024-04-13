open Asttypes

let rec_flag_to_yojson (rec_flag : rec_flag) : Yojson.Safe.t =
  (match rec_flag with
    | Nonrecursive ->
      `Assoc [
        ("tag", `String "Nonrecursive")
      ]
    | Recursive ->
      `Assoc [
        ("tag", `String "Recursive")
      ]
  )

let direction_flag_to_yojson (direction_flag : direction_flag) : Yojson.Safe.t =
  (match direction_flag with
    | Upto ->
      `Assoc [
        ("tag", `String "Upto")
      ]
    | Downto ->
      `Assoc [
        ("tag", `String "Downto")
      ]
  )

let private_flag_to_yojson (private_flag : private_flag) : Yojson.Safe.t =
  (match private_flag with
    | Private ->
      `Assoc [
        ("tag", `String "Private")
      ]
    | Public ->
      `Assoc [
        ("tag", `String "Public")
      ]
  )

let mutable_flag_to_yojson (mutable_flag : mutable_flag) : Yojson.Safe.t =
  (match mutable_flag with
    | Immutable ->
      `Assoc [
        ("tag", `String "Immutable")
      ]
    | Mutable ->
      `Assoc [
        ("tag", `String "Mutable")
      ]
  )

let virtual_flag_to_yojson (virtual_flag : virtual_flag) : Yojson.Safe.t =
  (match virtual_flag with
    | Virtual ->
      `Assoc [
        ("tag", `String "Virtual")
      ]
    | Concrete ->
      `Assoc [
        ("tag", `String "Concrete")
      ]
  )

let override_flag_to_yojson (override_flag : override_flag) : Yojson.Safe.t =
  (match override_flag with
    | Override ->
      `Assoc [
        ("tag", `String "Override")
      ]
    | Fresh ->
      `Assoc [
        ("tag", `String "Fresh")
      ]
  )

let closed_flag_to_yojson (closed_flag : closed_flag) : Yojson.Safe.t =
  (match closed_flag with
    | Closed ->
      `Assoc [
        ("tag", `String "Closed")
      ]
    | Open ->
      `Assoc [
        ("tag", `String "Open")
      ]
  )

let label_to_yojson (label : label) : Yojson.Safe.t = `String label

let arg_label_to_yojson (arg_label : arg_label) : Yojson.Safe.t =
  (match arg_label with
    | Nolabel ->
      `Assoc [
        ("tag", `String "Nolabel")
      ]
    | Labelled s ->
      `Assoc [
        ("tag", `String "Labelled");
        ("contents", `String s)
      ]
    | Optional s ->
      `Assoc [
        ("tag", `String "Optional");
        ("contents", `String s)
      ]
  )

(* Type Loc *)

let loc_to_yojson (a_to_yojson : 'a -> Yojson.Safe.t) (loc : 'a loc) : Yojson.Safe.t =
  `Assoc [
      ("tag", `String "Loc");
      ("txt", a_to_yojson loc.txt);
      ("loc", Location_serializer.t_to_yojson loc.loc)
    ]

let variance_to_yojson (variance : variance) : Yojson.Safe.t =
  (match variance with
    | Covariant ->
      `Assoc [
        ("tag", `String "Covariant")
      ]
    | Contravariant ->
      `Assoc [
        ("tag", `String "Contravariant")
      ]
    | Invariant ->
      `Assoc [
        ("tag", `String "Invariant")
      ]
  )
