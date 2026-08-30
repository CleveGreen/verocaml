type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let id x = x
let copy x = let y = x in y
let choose flag left right = if flag then left else right
let pair value count = (value, count)
let first pair = let value, _ = pair in value
let relay value = first (pair (choose true (copy (id value)) value) 7)

let labelled ~first ~second ~flag = if flag then first else second

let pick fallback ?(value = fallback) () =
  [%verocaml.ensures fun result -> result = value];
  value

let concrete (value : int) = value
let use_concrete (value : int) = concrete value

let use_int (value : int) = relay value
let use_bool (value : bool) = relay value
let use_abstract value = relay value

let use_labels (value : bool) =
  labelled ~second:false ~flag:true ~first:value

let omitted () =
  [%verocaml.ensures fun result -> result = 7];
  pick 7 ()

let supplied () =
  [%verocaml.ensures fun result -> result = 9];
  pick 7 ~value:9 ()

let forwarded (carrier : int option) = pick 7 ?value:carrier ()

let forwarded_absent () =
  [%verocaml.ensures fun result -> result = 7];
  pick 7 ?value:None ()

let forwarded_present () =
  [%verocaml.ensures fun result -> result = 9];
  pick 7 ?value:(Some 9) ()
