type t

val eval : t -> bool
val of_channel : in_channel -> t
val to_string : t -> string
