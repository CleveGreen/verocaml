let expected_int_size = 63

(* These are deliberately decimal literals parsed by Zarith.  They must not
   inherit the host's machine-integer representation. *)
let minimum = Z.of_string "-4611686018427387904"
let maximum = Z.of_string "4611686018427387903"

let check_target ?(int_size = Sys.int_size) () =
  if Int.equal int_size expected_int_size then Ok ()
  else
    Error
      (Diagnostic.make
         (Unsupported_target { expected_int_size; actual_int_size = int_size })
         (Diagnostic.file_span "<startup>"))
