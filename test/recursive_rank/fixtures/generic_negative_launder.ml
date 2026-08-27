type 'a sink = Sink of (('a * int) -> int)
type 'a sink_alias = 'a sink
type t = Ground | Bad of t sink_alias
