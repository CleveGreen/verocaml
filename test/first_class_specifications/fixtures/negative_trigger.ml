let closure_literal_trigger (value : int) : int =
  [%verocaml.requires
    forall (fun (f : int -> int) ->
        ((fun argument -> f argument) value [@trigger]) = f value)];
  value
