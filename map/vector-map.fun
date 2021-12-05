functor VectorMap(O : ORDER) :> MAP
where type key = O.t = struct

type key = O.t
type 'a t = (key * 'a) vector

fun empty () = Vector.fromList []
fun singleton kv = Vector.fromList [kv]
fun fromList l = Vector.fromList (Util.mergesort
   (fn ((k1, _), (k2, _)) => O.cmp (k1, k2)) l)
val size = Vector.length

fun binarySearchTail (m, k, i, j) =
   if i >= j
   then (j, false)
   else
      let
         val mid = i + (j - i) div 2
         val (k0, _) = Vector.sub (m, mid)
      in
         case O.cmp (k, k0) of
              LESS => binarySearchTail (m, k, i, mid)
            | EQUAL => (mid, true)
            | GREATER => binarySearchTail (m, k, mid + 1, j)
      end

fun binarySearch (m, k) = binarySearchTail (m, k, 0, Vector.length m)

fun lookup (m, k) =
let
   val (i, found) = binarySearch (m, k)
in
   if found then SOME (#2 (Vector.sub (m, i))) else NONE
end

fun insert (m, k, v) =
let
   val (i, found) = binarySearch (m, k)
in
   if found
   then Vector.update (m, i, (k, v))
   else Vector.tabulate (
   Vector.length m + 1,
   fn j => case Int.compare (j, i) of
                LESS => Vector.sub (m, j)
              | EQUAL => (k, v)
              | GREATER => Vector.sub (m, j - 1))
end

fun delete (m, k) =
let
   val (i, found) = binarySearch (m, k)
in
   if found
   then Vector.tabulate (
   Vector.length m - 1,
   fn j => if j < i then Vector.sub (m, j) else Vector.sub (m, j + 1))
   else m
end

fun foldl f = Vector.foldl (fn ((k, v), b) => f (k, v, b))

val app = Vector.app

end
