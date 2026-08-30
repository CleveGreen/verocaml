let contains_substring text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec search offset =
    offset + substring_length <= text_length
    &&
    (String.equal (String.sub text offset substring_length) substring
    || search (offset + 1))
  in
  search 0

let reject_delator_reference rendered =
  if contains_substring rendered "Delator" then
    failwith "the public PPX introduced a Delator reference into user code"

let implementation () =
  Parse.implementation (Lexing.from_string "let identity value = value")
  |> Vero_ppx_driver.implementation
  |> Format.asprintf "%a" Pprintast.structure
  |> reject_delator_reference

let interface () =
  Parse.interface (Lexing.from_string "val identity : int -> int")
  |> Vero_ppx_driver.interface
  |> Format.asprintf "%a" Pprintast.signature
  |> reject_delator_reference

let () =
  implementation ();
  interface ()
