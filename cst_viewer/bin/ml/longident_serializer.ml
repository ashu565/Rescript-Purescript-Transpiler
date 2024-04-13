open Longident

let rec t_to_yojson (t : t) : Yojson.Safe.t =
  match t with
  | Lident s ->
    `Assoc [
      ("tag", `String "Lident");
      ("contents", `String s)
    ]
  | Ldot (t, s) ->
    `Assoc [
      ("tag", `String "Ldot");
      ("contents", `List [t_to_yojson t; `String s])
    ]
  | Lapply (t1, t2) ->
    `Assoc [
      ("tag", `String "Lapply");
      ("contents", `List [t_to_yojson t1; t_to_yojson t2])
    ]