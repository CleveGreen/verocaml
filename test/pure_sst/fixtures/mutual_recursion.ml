let rec even (n : int) = n = 0 || odd (n - 1)
and odd (n : int) = n <> 0 && even (n - 1)
