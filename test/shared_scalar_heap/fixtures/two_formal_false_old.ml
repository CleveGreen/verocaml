type box = { mutable value : int }

let update_with_false_contract
    (x : box @ aliased) (y : box @ aliased) : unit =
  [%verocaml.requires
    -2_305_843_009_213_693_950 <= x.value
    && x.value <= 2_305_843_009_213_693_949
    && -2_305_843_009_213_693_950 <= y.value
    && y.value <= 2_305_843_009_213_693_949];
  [%verocaml.ensures fun _ ->
    y.value = [%verocaml.old y.value] + 2];
  x.value <- x.value + y.value;
  y.value <- y.value + 2
