module Imported = Numeric_collision_dependency

module Numeric_collision_dependency = struct
  type carrier = int
  let law value = value
end

let local_operation (value : Numeric_collision_dependency.carrier) = value
let imported_operation (value : Imported.carrier) = value

open Numeric_source_provider.Nested
let opened_operation (value : carrier) = law value

module Enclosing = struct
  type carrier = int
  let law value = value
  module Child = struct
    let operation (value : carrier) = law value
  end
end
