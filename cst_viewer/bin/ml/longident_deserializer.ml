open Longident

let rec t_of_yojson (json : Yojson.Safe.t) : t =
  match json with
  | `Assoc [("tag", `String "Lident"); ("contents", `String s)] -> Lident s
  | `Assoc [("tag", `String "Ldot"); ("contents", `List [t_json; `String s])] ->
    let t = t_of_yojson t_json in
    Ldot (t, s)
  | `Assoc [("tag", `String "Lapply"); ("contents", `List [t1_json; t2_json])] ->
    let t1 = t_of_yojson t1_json in
    let t2 = t_of_yojson t2_json in
    Lapply (t1, t2)
    | _ -> failwith "Invalid JSON format for Longident.t"