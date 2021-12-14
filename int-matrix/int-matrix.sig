signature INT_MATRIX =
sig

(* n by n matrices of integers *)
type t
val size: t -> int
val tabulate: int * (int * int -> IntInf.int) -> t
val sub: t * int * int -> IntInf.int
val mul: t * t -> t
val mulVec: t * IntInf.int vector -> IntInf.int vector
val powMulVec: t * int * IntInf.int vector -> IntInf.int vector

end
