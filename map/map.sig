signature MAP = sig

type key
type 'a t

val empty: unit -> 'a t
val singleton: key * 'a -> 'a t
val fromList: (key * 'a) list -> 'a t
val size: 'a t -> int

val lookup: 'a t * key -> 'a option

val insert: 'a t * key * 'a -> 'a t
val delete: 'a t * key -> 'a t

val foldl: (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
val app: (key * 'a -> unit) -> 'a t -> unit

end
