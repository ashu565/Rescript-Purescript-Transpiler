open Asttypes

let rec_flag_of_yojson (json : Yojson.Safe.t) : rec_flag =
  match json with
  | `Assoc [("tag", `String "Nonrecursive")] -> Nonrecursive
  | `Assoc [("tag", `String "Recursive")] -> Recursive
  | _ -> failwith "Invalid JSON format for rec_flag"


let direction_flag_of_yojson (json : Yojson.Safe.t) : direction_flag =
  match json with
  | `Assoc [("tag", `String "Upto")] -> Upto
  | `Assoc [("tag", `String "Downto")] -> Downto
  | _ -> failwith "Invalid JSON format for direction_flag"


let private_flag_of_yojson (json : Yojson.Safe.t) : private_flag =
  match json with
  | `Assoc [("tag", `String "Private")] -> Private
  | `Assoc [("tag", `String "Public")] -> Public
  | _ -> failwith "Invalid JSON format for private_flag"


let mutable_flag_of_yojson (json : Yojson.Safe.t) : mutable_flag =
  match json with
  | `Assoc [("tag", `String "Immutable")] -> Immutable
  | `Assoc [("tag", `String "Mutable")] -> Mutable
  | _ -> failwith "Invalid JSON format for mutable_flag"

let virtual_flag_of_yojson (json : Yojson.Safe.t) : virtual_flag =
  match json with
  | `Assoc [("tag", `String "Virtual")] -> Virtual
  | `Assoc [("tag", `String "Concrete")] -> Concrete
  | _ -> failwith "Invalid JSON format for virtual_flag"

let override_flag_of_yojson (json : Yojson.Safe.t) : override_flag =
  match json with
  | `Assoc [("tag", `String "Override")] -> Override
  | `Assoc [("tag", `String "Fresh")] -> Fresh
  | _ -> failwith "Invalid JSON format for override_flag"

let closed_flag_of_yojson (json : Yojson.Safe.t) : closed_flag =
  match json with
  | `Assoc [("tag", `String "Closed")] -> Closed
  | `Assoc [("tag", `String "Open")] -> Open
  | _ -> failwith "Invalid JSON format for closed_flag"

let label_of_yojson (json : Yojson.Safe.t) : label =
  match json with
  | `String s -> s
  | _ -> failwith "Invalid JSON format for label"

let arg_label_of_yojson (json : Yojson.Safe.t) : arg_label =
  match json with
  | `Assoc [("tag", `String "Nolabel")] -> Nolabel
  | `Assoc [("tag", `String "Labelled"); ("contents", `String s)] -> Labelled s
  | `Assoc [("tag", `String "Optional"); ("contents", `String s)] -> Optional s
  | _ -> failwith "Invalid JSON format for arg_label"

let loc_of_yojson (a_of_yojson : Yojson.Safe.t -> 'a) (json : Yojson.Safe.t) : 'a loc =
  let open Location in
  match json with
  | `Assoc [
      ("tag", `String "Loc");
      ("txt", txt_json);
      ("loc", loc_json)
    ] ->
    {
      txt = a_of_yojson txt_json;
      loc = Location_deserializer.t_of_yojson loc_json;
    }
  | _ -> failwith "Invalid JSON format for loc"

let variance_of_yojson (json : Yojson.Safe.t) : variance =
  match json with
  | `Assoc [("tag", `String "Covariant")] -> Covariant
  | `Assoc [("tag", `String "Contravariant")] -> Contravariant
  | `Assoc [("tag", `String "Invariant")] -> Invariant
  | _ -> failwith "Invalid JSON format for variance"