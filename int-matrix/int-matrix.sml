structure IntMatrix :> INT_MATRIX =
struct

exception DimensionMismatch

datatype t = T of IntInf.int vector * int

fun size (T (_, n)) = n

fun tabulate (n, f) =
   T (Vector.tabulate (n * n, fn i => f (i div n, i mod n)), n)

fun sub (T (v, n), i, j) =
   if 0 <= i andalso i < n andalso 0 <= j andalso j < n
   then Vector.sub (v, i * n + j)
   else raise Subscript

fun sumTail (n, f, s) =
   if n < 0
   then s
   else sumTail (n - 1, f, s + f n)

fun sum (n, f) = sumTail (n - 1, f, 0)

fun mul (mat1, mat2) =
   if size mat1 = size mat2
   then tabulate (size mat1, fn (i,j) =>
   sum (size mat1, fn k => sub (mat1, i, k) * sub (mat2, k, j)))
   else raise DimensionMismatch

fun mulVec (mat, vec) =
   if size mat = Vector.length vec
   then Vector.tabulate (size mat, fn i =>
   sum (size mat, fn j => sub (mat, i, j) * Vector.sub (vec, j)))
   else raise DimensionMismatch

fun powMulVec (mat, k, vec) =
   if k <= 0
   then vec
   else if k mod 2 = 0
   then powMulVec (mul (mat, mat), k div 2, vec)
   else powMulVec (mul (mat, mat), k div 2, mulVec (mat, vec))

end
