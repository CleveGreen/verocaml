let rec bad : 'a. 'a -> unit =
 fun value -> if true then () else bad (value, value)
