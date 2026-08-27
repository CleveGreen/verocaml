type 'a cell = { mutable value : 'a }
let set cell value = cell.value <- value
