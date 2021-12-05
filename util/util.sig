signature UTIL = sig

val readInputs: unit -> string list
val readInputsInt: unit -> int list
val intFromStringUnsafe: string -> int
val mergesort: ('a * 'a -> order) -> 'a list -> 'a list

end
