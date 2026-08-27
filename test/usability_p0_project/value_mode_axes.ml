let printed printer value = Format.asprintf "%a" printer value

let axis_name : type axis. axis Mode.Value.Axis.t -> string = function
  | Mode.Value.Axis.Comonadic Mode.Axis.Areality -> "Areality"
  | Mode.Value.Axis.Comonadic Mode.Axis.Forkable -> "Forkable"
  | Mode.Value.Axis.Comonadic Mode.Axis.Yielding -> "Yielding"
  | Mode.Value.Axis.Comonadic Mode.Axis.Linearity -> "Linearity"
  | Mode.Value.Axis.Comonadic Mode.Axis.Statefulness -> "Statefulness"
  | Mode.Value.Axis.Comonadic Mode.Axis.Portability -> "Portability"
  | Mode.Value.Axis.Monadic Mode.Axis.Uniqueness -> "Uniqueness"
  | Mode.Value.Axis.Monadic Mode.Axis.Visibility -> "Visibility"
  | Mode.Value.Axis.Monadic Mode.Axis.Contention -> "Contention"
  | Mode.Value.Axis.Monadic Mode.Axis.Staticity -> "Staticity"

let value_axis_state : type axis.
    axis Mode.Value.Axis.t -> Mode.Value.Const.t -> string =
 fun axis mode ->
  match axis with
  | Mode.Value.Axis.Comonadic Mode.Axis.Areality ->
      printed Mode.Regionality.Const.print mode.areality
  | Mode.Value.Axis.Comonadic Mode.Axis.Forkable ->
      printed Mode.Forkable.Const.print mode.forkable
  | Mode.Value.Axis.Comonadic Mode.Axis.Yielding ->
      printed Mode.Yielding.Const.print mode.yielding
  | Mode.Value.Axis.Comonadic Mode.Axis.Linearity ->
      printed Mode.Linearity.Const.print mode.linearity
  | Mode.Value.Axis.Comonadic Mode.Axis.Statefulness ->
      printed Mode.Statefulness.Const.print mode.statefulness
  | Mode.Value.Axis.Comonadic Mode.Axis.Portability ->
      printed Mode.Portability.Const.print mode.portability
  | Mode.Value.Axis.Monadic Mode.Axis.Uniqueness ->
      printed Mode.Uniqueness.Const.print mode.uniqueness
  | Mode.Value.Axis.Monadic Mode.Axis.Visibility ->
      printed Mode.Visibility.Const.print mode.visibility
  | Mode.Value.Axis.Monadic Mode.Axis.Contention ->
      printed Mode.Contention.Const.print mode.contention
  | Mode.Value.Axis.Monadic Mode.Axis.Staticity ->
      printed Mode.Staticity.Const.print mode.staticity

let expected_axes =
  [
    "Areality";
    "Forkable";
    "Yielding";
    "Linearity";
    "Statefulness";
    "Portability";
    "Uniqueness";
    "Visibility";
    "Contention";
    "Staticity";
  ]

let expected_states =
  [
    ("Areality", [ "global"; "local"; "regional" ]);
    ("Forkable", [ "forkable"; "unforkable" ]);
    ("Yielding", [ "unyielding"; "yielding" ]);
    ("Linearity", [ "many"; "once" ]);
    ("Statefulness", [ "observing"; "stateful"; "stateless" ]);
    ("Portability", [ "nonportable"; "portable"; "shareable" ]);
    ("Uniqueness", [ "aliased"; "unique" ]);
    ("Visibility", [ "immutable"; "read"; "read_write" ]);
    ("Contention", [ "contended"; "shared"; "uncontended" ]);
    ("Staticity", [ "dynamic"; "static" ]);
  ]

let expected_combinations =
  [
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=observing,Portability=shareable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=immutable,Contention=contended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read,Contention=shared,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=contended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=shared,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=static";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=unique,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=portable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=shareable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=many,Statefulness=stateless,Portability=portable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=unyielding,Linearity=once,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=forkable,Yielding=yielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=global,Forkable=unforkable,Yielding=unyielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=local,Forkable=unforkable,Yielding=yielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
    "Areality=regional,Forkable=unforkable,Yielding=yielding,Linearity=many,Statefulness=stateful,Portability=nonportable,Uniqueness=aliased,Visibility=read_write,Contention=uncontended,Staticity=dynamic";
  ]

let axes () =
  List.map (fun (Mode.Value.Axis.P axis) -> axis_name axis) Mode.Value.Axis.all

let value_combination mode =
  Mode.Value.Axis.all
  |> List.map (fun (Mode.Value.Axis.P axis) ->
      Printf.sprintf "%s=%s" (axis_name axis) (value_axis_state axis mode))
  |> String.concat ","

let alloc_combination (mode : Mode.Alloc.Const.t) =
  String.concat ","
    [
      "Areality=" ^ printed Mode.Locality.Const.print mode.areality;
      "Forkable=" ^ printed Mode.Forkable.Const.print mode.forkable;
      "Yielding=" ^ printed Mode.Yielding.Const.print mode.yielding;
      "Linearity=" ^ printed Mode.Linearity.Const.print mode.linearity;
      "Statefulness=" ^ printed Mode.Statefulness.Const.print mode.statefulness;
      "Portability=" ^ printed Mode.Portability.Const.print mode.portability;
      "Uniqueness=" ^ printed Mode.Uniqueness.Const.print mode.uniqueness;
      "Visibility=" ^ printed Mode.Visibility.Const.print mode.visibility;
      "Contention=" ^ printed Mode.Contention.Const.print mode.contention;
      "Staticity=" ^ printed Mode.Staticity.Const.print mode.staticity;
    ]
