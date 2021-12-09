structure IndexSet = RedBlackSet (
   structure Elem = PairOrdered (
      structure Ordered1 = IntOrdered
      structure Ordered2 = IntOrdered
   )
)

fun parseLine s =
   Vector.tabulate (String.size s - 1,
   fn i => ord (String.sub (s, i)) - ord #"0")

fun readInputMatrix () =
   Matrix.fromRows (vector (map parseLine (Util.readInputs ())))

fun isLowPoint (m, i, j) =
let
   val mij = Matrix.sub (m, i, j)
   val up = Matrix.subDefault (m, i - 1, j, 10)
   val down = Matrix.subDefault (m, i + 1, j, 10)
   val right = Matrix.subDefault (m, i, j + 1, 10)
   val left = Matrix.subDefault (m, i, j - 1, 10)
in
   mij < up andalso mij < down andalso mij < right andalso mij < left
end

fun risk (m, i, j) =
   if isLowPoint (m, i, j)
   then Matrix.sub (m, i, j) + 1
   else 0

fun riskMatrix mat =
let
   val (m, n) = Matrix.size mat
in
   Matrix.tabulate (m, n, fn (i, j) => risk (mat, i, j))
end

val matrixSum = Matrix.fold (op +) 0

fun basinSizeDfs (explored, seen0, stackStart, mat) =
   case stackStart of
        (i, j) :: stack0 =>
        if IndexSet.member explored (i, j)
        then basinSizeDfs (explored, seen0, stackStart, mat)
        else
           let
              val (seen1, stack1) =
                 if not (IndexSet.member seen0 (i - 1, j)) andalso
                 Matrix.subDefault (mat, i - 1, j, 10) < 9
                 then (IndexSet.insert seen0 (i - 1, j), (i - 1, j) :: stack0)
                 else (seen0, stack0)
              val (seen2, stack2) =
                 if not (IndexSet.member seen1 (i + 1, j)) andalso
                 Matrix.subDefault (mat, i + 1, j, 10) < 9
                 then (IndexSet.insert seen1 (i + 1, j), (i + 1, j) :: stack1)
                 else (seen1, stack1)
              val (seen3, stack3) =
                 if not (IndexSet.member seen2 (i, j + 1)) andalso
                 Matrix.subDefault (mat, i, j + 1, 10) < 9
                 then (IndexSet.insert seen2 (i, j + 1), (i, j + 1) :: stack2)
                 else (seen2, stack2)
              val (seen4, stack4) =
                 if not (IndexSet.member seen3 (i, j - 1)) andalso
                 Matrix.subDefault (mat, i, j - 1, 10) < 9
                 then (IndexSet.insert seen3 (i, j - 1), (i, j - 1) :: stack3)
                 else (seen3, stack3)
           in
              basinSizeDfs (
              IndexSet.insert explored (i, j), seen4, stack4, mat)
           end
      | [] => IndexSet.size explored

fun basinSize (mat, i, j) =
   basinSizeDfs (IndexSet.empty, IndexSet.singleton (i, j), [(i, j)], mat)

fun basinSizeMatrix mat =
let
   val (m, n) = Matrix.size mat
in
   Matrix.tabulate (m, n,
   fn (i, j) => if isLowPoint (mat, i, j) then basinSize (mat, i, j) else 0)
end

val topThreeBasins =
   Matrix.fold (fn (x, (b3, b2, b1)) =>
   if x <= b3
   then (b3, b2, b1)
   else if x <= b2
   then (x, b2, b1)
   else if x <= b1
   then (b2, x, b1)
   else (b2, b1, x)
   ) (0, 0, 0)

val () =
   let
      val mat = readInputMatrix ()
      val part1 = matrixSum (riskMatrix mat)
      val () = print (Int.toString part1 ^ "\n")
      val part2 =
         let
            val (b3, b2, b1) = topThreeBasins (basinSizeMatrix mat)
         in
            b3 * b2 * b1
         end
      val () = print (Int.toString part2 ^ "\n")
   in
      ()
   end
