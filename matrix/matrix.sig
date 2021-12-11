signature MATRIX =
sig

type 'a t
val size: 'a t -> int * int
val tabulate: int * int * (int * int -> 'a) -> 'a t
val fromRows: 'a vector vector -> 'a t
val fromColumns: 'a vector vector -> 'a t
val sub: 'a t * int * int -> 'a
val subDefault: 'a t * int * int * 'a -> 'a
val update: 'a t * int * int * ('a -> 'a) -> 'a t
val fold: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
val map: 'a t * ('a -> 'b) -> 'b t
val indicesSuchThat: 'a t * ('a -> bool) -> (int * int) list

end
