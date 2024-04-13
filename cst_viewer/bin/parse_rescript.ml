open Lexing
open Parsetree
open LexRescript
open ParserRescript

let parse_file "sample_rescript.res" =
  let ic = open_in file_name in
  let lexbuf = Lexing.from_channel ic in
  try
    let parsed_ast = ParserRescript.implementation LexRescript.token lexbuf in
    close_in ic;
    parsed_ast
  with
  | ParserRescript.Error ->
      Printf.eprintf "Syntax error at position %d.\n" (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol);
       exit 1
  | _ ->
      Printf.eprintf "An error occurred during parsing.\n";
      exit 1

let rec print_structure_item item =
  let json_node =
    match item.pstr_desc with
    | Pstr_eval (expr, _) ->
        Printf.sprintf "{ \"type\": \"expression\", \"content\": \"%s\" }" (Pprintast.string_of_expression expr)
    (* Add more cases for other kinds of structure items as needed *)
    | _ ->
        Printf.sprintf "{ \"type\": \"unknown\" }"
  in
  print_endline json_node

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <rescript_file>\n" Sys.argv.(0);
    exit 1
  );
  let file_name = Sys.argv.(1) in
  let parsed_ast = parse_file file_name in
  List.iter print_structure_item parsed_ast
