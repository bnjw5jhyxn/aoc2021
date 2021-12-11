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

fun update (T (v, m, n), i, j, f) =
   if 0 <= i andalso i < m andalso 0 <= j andalso j < n
   then T (Vector.update (v, i * n + j, f (Vector.sub (v, i * n + j))), m, n)
   else raise Fail ("(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ")")

fun fold f b (T (v, _, _)) = Vector.foldl f b v

fun map (T (v, m, n), f) = T (Vector.map f v, m, n)

fun indicesSuchThat (T (v, m, n), f) =
   Vector.foldri
   (fn (idx, x, acc) =>
   if f x
   then (Int.quot (idx, n), Int.rem (idx, n)) :: acc
   else acc)
   [] v

end
