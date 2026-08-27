let rec missing_decreases (n : int) =
  if n = 0 then 0 else missing_decreases (n - 1)
