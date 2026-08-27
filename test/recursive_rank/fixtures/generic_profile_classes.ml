type 'a prohibited = Prohibited of ('a -> int)
type 'a dependent = Dependent of 'a
type 'a delayed = Delayed of (unit -> 'a)
type 'a independent = Independent_ground | Independent_value of 'a
