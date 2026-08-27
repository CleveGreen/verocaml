let observe (stack : Model_dependency.Stack.t @ read) =
  [%verocaml.ensures fun _result -> false];
  Model_dependency.Stack.model stack
