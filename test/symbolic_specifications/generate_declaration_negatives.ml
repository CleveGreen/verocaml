let cases =
  [ ("empty", "[%%verocaml.symbolic]\n")
  ; ("multiple", "[%%verocaml.symbolic val a : int val b : int]\n")
  ; ("body", "[%%verocaml.symbolic let bad = 1]\n")
  ; ("recursive", "[%%verocaml.symbolic let rec bad x = bad x]\n")
  ; ("contract", "[%%verocaml.symbolic val bad : int [@@verocaml.spec]]\n")
  ; ("primitive", "[%%verocaml.symbolic external bad : int = \"bad\"]\n")
  ; ("payload", "[%%verocaml.symbolic \"bad\"]\n")
  ]

let write_fixture output_directory (name, source) =
  let path = Filename.concat output_directory ("declaration_" ^ name ^ ".ml") in
  let output = open_out_bin path in
  output_string output source;
  close_out output

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: generate_declaration_negatives OUTPUT_DIRECTORY";
    exit 2);
  List.iter (write_fixture Sys.argv.(1)) cases
