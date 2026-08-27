let reject_imported (cell : Imported_box.box @ aliased) : unit =
  cell.value <- 1
