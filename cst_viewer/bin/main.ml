(* START - TO GET THE PARSE TREE OF ANY RESCRIPT FILE *)

(*
let store_person_as_json_file filename person =
  let json_str = Yojson.Safe.to_string (Parsetree_serializer.structure_to_yojson person) in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc


let parse_file file_name =
  let ic = open_in file_name in
  let _lexbuf = Lexing.from_channel ic in
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  store_person_as_json_file "sample_rescript.json" parseResult.parsetree
  
let () =
  let file_name = "sample_rescript.res" in
  (* let parsed_ast = parse_file file_name in *)
  (* List.iter print_structure_item parsed_ast *)
  parse_file file_name

*)

(* END - TO GET THE PARSE TREE OF ANY RESCRIPT FILE *)

(* START - TO GET THE PRETTY PRINT OF ANY PARSE TREE *)

let () = 
  let json = Yojson.Safe.from_file "sample_rescript.json" in
  let sample_rescript_json = Parsetree_deserializer2.structure_of_yojson json in
  Res_driver.printEngine.printImplementation
    ~width:4
    ~filename:"xxx.res"
    ~comments:[]
    sample_rescript_json;
  print_endline "Done!"


(* END - TO GET THE PRETTY PRINT OF ANY PARSE TREE *)

(* (* open Res_driver
open Parsetree
open Yojson
(* open Yojson.Basic *)

let store_person_as_json_file filename person =
  let json_str = Yojson.Safe.to_string (Parsetree.structure_to_yojson person) in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc

let parse_file file_name =
  let ic = open_in file_name in
  let _lexbuf = Lexing.from_channel ic in
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  store_person_as_json_file "res.json" parseResult.parsetree
  
let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <rescript_file>\n" Sys.argv.(0);
    exit 1
  );
  let file_name = Sys.argv.(1) in
  Printf.eprintf "Filename: %s \n" file_name;
  (* let parsed_ast = parse_file file_name in *)
  (* List.iter print_structure_item parsed_ast *)
  parse_file file_name *)


(* Define an OCaml type *)
(* open Yojson *)

(* Define an OCaml type *)

(* Define a nested type *)

(* open Person_j *)
open Person
open Parsetree
open Parsetree_serializer
open Res_driver

let my_name = { firstName = "Alice"; lastName = "Smith" }
let _my_person =
  { name = my_name;
    age =
      match Some 5 with
      | None -> 10
      | Some u -> u
  }

let _my_constant = Pconst_float ("5.0", Some 'f')

let _my_attribute : attribute = ({
    txt : string = "Hello";
    loc = {
      loc_start = {pos_fname = "file"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
      loc_end = {pos_fname = "file"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0};
      loc_ghost = false
    }
  }, PSig [])

let store_person_as_json_file filename result =
  let json_str = Yojson.Safe.to_string (structure_to_yojson result) in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc

(* Usage *)
(* let () = store_person_as_json_file "person.json" my_person *)


let parse_file file_name =
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  store_person_as_json_file "../haskell_rescript_types/res.json" parseResult.parsetree

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <rescript_file>\n" Sys.argv.(0);
    exit 1
  );
  let file_name = Sys.argv.(1) in
  Printf.eprintf "Filename: %s \n" file_name;
  (* let parsed_ast = parse_file file_name in *)
  (* List.iter print_structure_item parsed_ast *)
  parse_file file_name *)

(* THIS IS START TESTING FOR PRETTY PRINT *)
(*
open Res_driver

let _store_string_as_file filename str _constant =
  let json_str = str in
  let oc = open_out filename in
  output_string oc json_str;
  close_out oc


(* let parse_file file_name =
  let ic = open_in file_name in
  let _lexbuf = Lexing.from_channel ic in
  let parseResult = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename:file_name in
  parseResult *)

(* let () =
  let parsetree = parse_file "sample_rescript.res" in
  Res_driver.printEngine.printImplementation
    ~width:4
    ~filename:"xxx.res"
    ~comments:[]
    parsetree.parsetree;
  print_endline "Done!"
*)
(* THIS IS END TESTING FOR PRETTY PRINT *)
(* open Yojson.Basic *)

(* This is for testing fromjson start here *)
(*
open Yojson.Basic.Util
open Printf

type author = {
  name : string;
  affiliation : string;
}

type books = {
  title : string;
  tags : string list;
  pages : int;
  authors : author list;
  is_online : bool option;
}

let author_of_json json =
  {
    name = json |> member "name" |> to_string;
    affiliation = json |> member "affiliation" |> to_string;
  }

let books_of_json json =
  {
    title = json |> member "title" |> to_string;
    tags = json |> member "tags" |> to_list |> filter_string;
    pages = json |> member "pages" |> to_int;
    authors = json |> member "authors" |> to_list |> List.map author_of_json;
    is_online = json |> member "is_online" |> to_bool_option;
  }


let () =
  try
    let json = Yojson.Basic.from_file "books.json" in
    let my_books = books_of_json json in
    printf "Title: %s (%d)\n" my_books.title my_books.pages;
    printf "Authors: %s\n" (String.concat ", " (List.map (fun a -> a.name ^ " (" ^ a.affiliation ^ ")") my_books.authors));  (* Include affiliation in output *)
    printf "Tags: %s\n" (String.concat ", " my_books.tags);
    let string_of_bool_option =
      function
      | None -> "<unknown>"
      | Some true -> "yes"
      | Some false -> "no" in
    printf "Online: %s\n" (string_of_bool_option my_books.is_online);
  with
  | Sys_error msg -> Printf.eprintf "Error: %s\n" msg

*)
(* This is for testing fromjson end here *)

(* open Yojson.Basic.Util *)

type personxx = {
name : string;
  age : int;
}
[@@deriving yojson]


let () = 
  let json = Yojson.Safe.from_file "constant.json" in
  let my_books = Parsetree_deserializer2.constant_of_yojson json in
  print_endline (Yojson.Safe.to_string (Parsetree_serializer.constant_to_yojson my_books)) *)

(* 
type test_type = {
  ptyp_desc: string;
  ptyp_loc: string;
  ptyp_attributes: string;
}

      

let test_type_of_yojson (json: Yojson.Safe.t) : test_type =
  match json with
  | `Assoc _fields ->
    begin match Yojson.Safe.Util.member "tag" json with
    | `String "TestType" ->
      {
        ptyp_desc = Yojson.Safe.Util.member "ptypDesc" json |> Yojson.Safe.Util.to_string;
        ptyp_loc = Yojson.Safe.Util.member "ptypLoc" json |> Yojson.Safe.Util.to_string;
        ptyp_attributes = Yojson.Safe.Util.member "ptypAttributes" json |> Yojson.Safe.Util.to_string;
      }
    | _ -> failwith "Invalid Tag"
    end
  | _ -> failwith "Invalid JSON"


let test_type_to_yojson (t: test_type) : Yojson.Safe.t =
  `Assoc [
    ("ptypDesc", `String t.ptyp_desc);
    ("ptypLoc", `String t.ptyp_loc);
    ("ptypAttributes", `String t.ptyp_attributes);
  ]

let () =
  let json = Yojson.Safe.from_file "test_type.json" in
  let my_test_type = test_type_of_yojson json in
  print_endline (Yojson.Safe.to_string (test_type_to_yojson my_test_type)) *)