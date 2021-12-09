structure Matrix :> MATRIX =
struct

(*
 * this structure represents an m by n matrix
 * (with m rows and n columns)
 * as a vector of length m*n in row-major order
 *)

datatype 'a t = T of ('a vector * int * int)

fun size (T (_, m, n)) = (m, n)

fun tabulate (m, n, f) =
   T (Vector.tabulate (m * n,
   fn i => f (Int.quot (i, n), Int.rem (i, n))), m, n)

fun fromRows v =
let
   val m = Vector.length v
   val n = Vector.length (Vector.sub (v, 0))
in
   tabulate (m, n, fn (i, j) => Vector.sub (Vector.sub (v, i), j))
end

fun fromColumns v =
let
   val m = Vector.length (Vector.sub (v, 0))
   val n = Vector.length v
in
   tabulate (m, n, fn (i, j) => Vector.sub (Vector.sub (v, j), i))
end

fun sub (T (v, m, n), i, j) =
   if 0 <= i andalso i < m andalso 0 <= j andalso j < n
   then Vector.sub (v, i * n + j)
   else raise Subscript

fun subDefault (T (v, m, n), i, j, x) =
   if 0 <= i andalso i < m andalso 0 <= j andalso j < n
   then Vector.sub (v, i * n + j)
   else x

fun fold f b (T (v, _, _)) = Vector.foldl f b v

end
