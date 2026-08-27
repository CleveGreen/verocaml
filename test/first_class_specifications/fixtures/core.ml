let apply (f : 'a -> 'b) (x : 'a) : 'b = f x [@@verocaml.spec]

let compose (f : 'b -> 'c) (g : 'a -> 'b) : 'a -> 'c = fun x -> f (g x)
[@@verocaml.spec]

let adder (x : int) : int -> int = fun y -> x + y [@@verocaml.spec]
let increment (x : int) : int = x + 1 [@@verocaml.spec]
let is_positive (x : int) : bool = x > 0 [@@verocaml.spec]
let some (x : 'a) : 'a option = Some x [@@verocaml.spec]

let option_present (value : 'a option) : bool =
  match value with None -> false | Some _ -> true
[@@verocaml.spec]

let negate (value : bool) : bool = not value [@@verocaml.spec]
let plus (left : int) (right : int) : int = left + right [@@verocaml.spec]

let labelled_plus ~(base : int) (value : int) : int = base + value
[@@verocaml.spec]

let captured_factory (f : int -> int) (offset : int) : int -> int =
 fun value -> f value + offset
[@@verocaml.spec]

let constant_factory (value : int) : int -> int = fun _ -> value
[@@verocaml.spec]

let returned_contract (value : int) : unit =
  [%verocaml.ensures fun _ -> adder value 2 = value + 2] ; ()
[@@verocaml.proof]

let verify_bool (x : int) : int =
  [%verocaml.assert apply is_positive x = (x > 0)];
  [%verocaml.assert compose negate is_positive x = not (x > 0)];
  [%verocaml.ensures fun result -> result = x];
  x

let verify_option (x : int) : int =
  [%verocaml.assert apply some x = Some x];
  [%verocaml.assert apply option_present (Some x)];
  [%verocaml.assert not (apply option_present (None : int option))];
  [%verocaml.ensures fun result -> result = x];
  x

let verify_if_merge (x : int) : int =
  [%verocaml.assert
    (let left = x + 2 in
     let right = x - 3 in
     let selected =
       if x >= 0 then (fun value -> value + left)
       else fun value -> right - value
     in
     selected 4)
    = if x >= 0 then x + 6 else x - 7];
  [%verocaml.ensures fun result -> result = x];
  x

let verify_match_merge (x : int) : int =
  [%verocaml.assert
    (let left = x + 5 in
     let right = x - 2 in
     let selected =
       match x >= 0 with
       | true -> fun value -> left - value
       | false -> fun value -> value + right
     in
     selected 3)
    = if x >= 0 then x + 2 else x + 1];
  [%verocaml.ensures fun result -> result = x];
  x

let verify_core (x : int) : int =
  [%verocaml.assert apply increment x = x + 1];
  [%verocaml.assert adder x 2 = x + 2];
  [%verocaml.assert apply (adder x) 3 = x + 3];
  [%verocaml.assert compose increment increment x = x + 2];
  [%verocaml.assert captured_factory increment 4 x = x + 5];
  [%verocaml.assert constant_factory x 99 = x];
  [%verocaml.assert
    (let alias = increment in
     alias x)
    = x + 1];
  [%verocaml.assert
    (let add_x = plus x in
     add_x 2)
    = x + 2];
  [%verocaml.assert
    (let add_x = labelled_plus ~base:x in
     add_x 3)
    = x + 3];
  [%verocaml.assert increment = increment];
  [%verocaml.assert adder x = adder x];
  [%verocaml.assert captured_factory increment x = captured_factory increment x];
  [%verocaml.assert plus x 2 = (plus x) 2];
  [%verocaml.ensures fun result -> result = x];
  x
