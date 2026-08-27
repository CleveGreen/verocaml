let () =
  let record = Provider.make_record 7 in
  let unique = Provider.make_unique_record 8 in
  let tuple_value, tuple_record, tuple_box = Provider.make_tuple 3 in
  Printf.printf "%d %d %d %d %d\n" record.Provider.value
    unique.Provider.value tuple_value tuple_record.Provider.value
    tuple_box.Provider.box_value
