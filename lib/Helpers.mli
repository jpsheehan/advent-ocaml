type jin_channel

val jin_string : string -> jin_channel
val jin_file : string -> jin_channel
val jin_stdin : unit -> jin_channel

val read_generic : jin_channel -> f:(string -> 'a option) -> 'a list
val read_as_ints : jin_channel -> int list
