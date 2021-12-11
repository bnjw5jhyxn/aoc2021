structure IndexSet = RedBlackSet (
   structure Elem = PairOrdered (
      structure Ordered1 = IntOrdered
      structure Ordered2 = IntOrdered
   )
)

fun getNeighbors (mat, i0, j0) =
let
   val (m, n) = Matrix.size mat
in
   List.filter
   (fn (i, j) => 0 <= i andalso i < m andalso 0 <= j andalso j < n)
   [(i0-1, j0-1), (i0-1, j0), (i0-1, j0+1), (i0, j0-1), (i0, j0+1),
   (i0+1, j0-1), (i0+1, j0), (i0+1, j0+1)]
end

fun incrementNeighbors (mat0, indices) =
   foldl
   (fn ((i,j), mat) => Matrix.update (mat, i, j, fn x => x + 1))
   mat0
   indices

fun propagateFlashesTail (energy, stack, flashed) =
   case stack of
        (i,j) :: stack1 =>
        let
           val neighbors =
              List.filter
              (fn idx => not (IndexSet.member flashed idx))
              (getNeighbors (energy, i, j))
           val energy1 = incrementNeighbors (energy, neighbors)
           val flashNbrs =
              List.filter
              (fn (ip,jp) => Matrix.sub (energy1, ip, jp) > 9)
              neighbors
        in
           propagateFlashesTail (
           energy1,
           flashNbrs @ stack1,
           foldl (fn (nbr, acc) => IndexSet.insert acc nbr) flashed flashNbrs)
        end
      | [] => (energy, flashed)

fun assert (b, s) = if b then () else raise Fail s

fun step energy0 =
let
   val energy1 = Matrix.map (energy0, fn x => x + 1)
   val initFlashes = Matrix.indicesSuchThat (energy1, fn x => x > 9)
   val (energy2, flashes) =
      propagateFlashesTail (
      energy1,
      initFlashes,
      foldl (fn (idx, acc) => IndexSet.insert acc idx)
      IndexSet.empty initFlashes)
   (*
   val () = assert (IndexSet.size flashes =
      Matrix.fold (fn (x, acc) => if x > 9 then acc + 1 else acc) 0 energy2,
      "incorrect number of flashes")
   *)
in
   (IndexSet.foldl (fn ((i,j), mat) => Matrix.update (mat, i, j, fn _ => 0))
   energy2 flashes,
   IndexSet.size flashes)
end

fun countFlashesTail (energy, numFlashes, stepsRemaining) =
   if stepsRemaining <= 0
   then numFlashes
   else
      let
         val (energy1, newFlashes) = step energy
      in
         countFlashesTail (energy1, numFlashes + newFlashes, stepsRemaining - 1)
      end

fun allFlashTail (energy, i) =
let
   val (energy1, flashes) = step energy
in
   if flashes = 100 then i else allFlashTail (energy1, i + 1)
end

fun parseLine s =
   Vector.tabulate (String.size s - 1,
   fn i => ord (String.sub (s, i)) - ord #"0")

val readInputMatrix =
   Matrix.fromRows o vector o (map parseLine) o Util.readInputs

val () =
   let
      val input = readInputMatrix ()
      val part1 = countFlashesTail (input, 0, 100)
      val () = print (Int.toString part1 ^ "\n")
      val part2 = allFlashTail (input, 1)
      val () = print (Int.toString part2 ^ "\n")
   in
      ()
   end
