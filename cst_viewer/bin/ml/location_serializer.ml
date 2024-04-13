open Location

(* type t = Warnings.loc = { loc_start: position; loc_end: position; loc_ghost: bool };; *)

let t_to_yojson (t : t) : Yojson.Safe.t =
  `Assoc [
    ("tag", `String "Location");
    ("locStart", `Assoc [
      ("tag", `String "Position");
      ("posFname", `String t.loc_start.pos_fname);
      ("posLnum", `Int t.loc_start.pos_lnum);
      ("posBol", `Int t.loc_start.pos_bol);
      ("posCnum", `Int t.loc_start.pos_cnum)
    ]);
    ("locEnd", `Assoc [
      ("tag", `String "Position");
      ("posFname", `String t.loc_start.pos_fname);
      ("posLnum", `Int t.loc_start.pos_lnum);
      ("posBol", `Int t.loc_start.pos_bol);
      ("posCnum", `Int t.loc_start.pos_cnum)
    ]);
    ("locGhost", `Bool t.loc_ghost);
  ]
