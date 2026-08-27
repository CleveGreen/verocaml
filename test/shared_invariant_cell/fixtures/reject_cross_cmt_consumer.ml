let cross_cmt_client
    (cell : Import_provider.Cell.t @ aliased) : int =
  Import_provider.Cell.increment cell;
  Import_provider.Cell.get cell
