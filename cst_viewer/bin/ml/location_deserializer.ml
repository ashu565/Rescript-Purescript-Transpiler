open Location

let t_of_yojson (json : Yojson.Safe.t) : t =
  let open Location in
  match json with
  | `Assoc [
      ("tag", `String "Location");
      ("locStart", `Assoc [
        ("tag", `String "Position");
        ("posFname", `String fname_start);
        ("posLnum", `Int lnum_start);
        ("posBol", `Int bol_start);
        ("posCnum", `Int cnum_start)
      ]);
      ("locEnd", `Assoc [
        ("tag", `String "Position");
        ("posFname", `String fname_end);
        ("posLnum", `Int lnum_end);
        ("posBol", `Int bol_end);
        ("posCnum", `Int cnum_end)
      ]);
      ("locGhost", `Bool ghost)
    ] ->
    {
      loc_start = {
        pos_fname = fname_start;
        pos_lnum = lnum_start;
        pos_bol = bol_start;
        pos_cnum = cnum_start;
      };
      loc_end = {
        pos_fname = fname_end;
        pos_lnum = lnum_end;
        pos_bol = bol_end;
        pos_cnum = cnum_end;
      };
      loc_ghost = ghost;
    }
  | _ -> failwith "Invalid JSON format for t"
