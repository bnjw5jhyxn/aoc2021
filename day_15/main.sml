structure Ipd = RedBlackDict (
   structure Key = PairOrdered (
      structure Ordered1 = IntOrdered
      structure Ordered2 = IntOrdered
   )
)

structure Iq = PairingPQueue (IntOrdered)

fun parseLine s =
   Vector.tabulate (String.size s - 1,
   fn i => ord (String.sub (s, i)) - ord #"0")

val readInputMatrix =
   Matrix.fromRows o vector o (map parseLine) o Util.readInputs

fun getNeighbors (mat, pt) =
let
   val (m, n) = Matrix.size mat
   val (i0, j0) = pt
in
   List.filter
   (fn (i, j) => 0 <= i andalso i < m andalso 0 <= j andalso j < n)
   [(i0-1, j0), (i0, j0-1), (i0, j0+1), (i0+1, j0)]
end

fun getNeighborsTile (mat, pt) =
let
   val (m, n) = Matrix.size mat
   val (i0, j0) = pt
in
   List.filter
   (fn (i, j) => 0 <= i andalso i < 5 * m andalso 0 <= j andalso j < 5 * n)
   [(i0-1, j0), (i0, j0-1), (i0, j0+1), (i0+1, j0)]
end

fun matrixSubTile (mat, pt) =
let
   val (m, n) = Matrix.size mat
   val (ibig, jbig) = pt
   val ti = ibig div m
   val i = ibig mod m
   val tj = jbig div n
   val j = jbig mod n
in
   (Matrix.sub (mat, i, j) + ti + tj - 1) mod 9 + 1
end

fun dijkstra (mat, finalDistances, queue0, goalPoint) =
let
   val ((d, pt), queue1) = Iq.deleteMin queue0
in
   if pt = goalPoint
   then d
   else if Ipd.member finalDistances pt
   then dijkstra (mat, finalDistances, queue1, goalPoint)
   else dijkstra (
      mat,
      Ipd.insert finalDistances pt d,
      foldl
         (fn ((i,j), acc) =>
            if Ipd.member finalDistances (i, j)
            then acc
            else Iq.insert acc (d + Matrix.sub (mat, i, j), (i, j)))
         queue1
         (getNeighbors (mat, pt)),
       goalPoint)
end

fun dijkstraTile (mat, finalDistances, queue0, goalPoint) =
let
   val ((d, pt), queue1) = Iq.deleteMin queue0
in
   if pt = goalPoint
   then d
   else if Ipd.member finalDistances pt
   then dijkstraTile (mat, finalDistances, queue1, goalPoint)
   else dijkstraTile (
      mat,
      Ipd.insert finalDistances pt d,
      foldl
         (fn (nbr, acc) =>
            if Ipd.member finalDistances nbr
            then acc
            else Iq.insert acc (d + matrixSubTile (mat, nbr), nbr))
         queue1
         (getNeighborsTile (mat, pt)),
       goalPoint)
end

val () =
   let
      val mat = readInputMatrix ()
      val (m, n) = Matrix.size mat
      val part1 = dijkstra (
         mat,
         Ipd.empty,
         Iq.singleton (0, (0,0)),
         (m - 1, n - 1))
      val () = print (Int.toString part1 ^ "\n")
      val part2 = dijkstraTile (
         mat,
         Ipd.empty,
         Iq.singleton (0, (0,0)),
         (5 * m - 1, 5 * n - 1))
      val () = print (Int.toString part2 ^ "\n")
   in
      ()
   end
