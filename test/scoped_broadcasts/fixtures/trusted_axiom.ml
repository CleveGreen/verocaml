let observed (_value : int) : bool = true [@@verocaml.spec]

let axiom_sugar (value : int) : unit =
  [%verocaml.ensures fun _ -> (not ((observed value) [@trigger])) || value = 0];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]

let axiom_long (value : int) : unit =
  [%verocaml.ensures fun _ -> (not ((observed value) [@trigger])) || value = 0];
  ()
[@@verocaml.proof] [@@verocaml.external_body] [@@verocaml.broadcast]

let ordinary_axiom (value : int) : unit =
  [%verocaml.ensures fun _ -> observed value || value = value];
  () [@@verocaml.axiom]

let sugar_use (value : int) : unit =
  [%verocaml.requires observed value];
  [%verocaml.requires
    forall (fun candidate -> ((observed candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.activate [axiom_sugar] ([%verocaml.assert value = 0]; ())]
[@@verocaml.proof]

let long_use (value : int) : unit =
  [%verocaml.requires observed value];
  [%verocaml.requires
    forall (fun candidate -> ((observed candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.activate [axiom_long] ([%verocaml.assert value = 0]; ())]
[@@verocaml.proof]

let ordinary_control (value : int) : unit =
  ordinary_axiom value;
  [%verocaml.assert value = value];
  () [@@verocaml.proof]

(* Matched broadcast callers differ only by declaration spelling and name. *)
