structure IntOrder: ORDER = struct
   type t = int
   val cmp = Int.compare
end

structure IntVectorMap = VectorMap(IntOrder)

val notdigit = not o Char.isDigit

fun readInputsBingoTail (boards, lns) =
   case lns of
        sepln :: l0 :: l1 :: l2 :: l3 :: l4 :: remlns =>
        let
           val (x00, x01, x02, x03, x04) =
              case map Util.intFromStringUnsafe (String.tokens notdigit l0) of
                   [a0, a1, a2, a3, a4] => (a0, a1, a2, a3, a4)
                 | _ => raise Fail "parse row 0"
           val (x10, x11, x12, x13, x14) =
              case map Util.intFromStringUnsafe (String.tokens notdigit l1) of
                   [a0, a1, a2, a3, a4] => (a0, a1, a2, a3, a4)
                 | _ => raise Fail "parse row 1"
           val (x20, x21, x22, x23, x24) =
              case map Util.intFromStringUnsafe (String.tokens notdigit l2) of
                   [a0, a1, a2, a3, a4] => (a0, a1, a2, a3, a4)
                 | _ => raise Fail "parse row 2"
           val (x30, x31, x32, x33, x34) =
              case map Util.intFromStringUnsafe (String.tokens notdigit l3) of
                   [a0, a1, a2, a3, a4] => (a0, a1, a2, a3, a4)
                 | _ => raise Fail "parse row 3"
           val (x40, x41, x42, x43, x44) =
              case map Util.intFromStringUnsafe (String.tokens notdigit l4) of
                   [a0, a1, a2, a3, a4] => (a0, a1, a2, a3, a4)
                 | _ => raise Fail "parse row 4"
           val b = IntVectorMap.fromList [
           (x00, (0,0)), (x01, (0,1)), (x02, (0,2)), (x03, (0,3)), (x04, (0,4)),
           (x10, (1,0)), (x11, (1,1)), (x12, (1,2)), (x13, (1,3)), (x14, (1,4)),
           (x20, (2,0)), (x21, (2,1)), (x22, (2,2)), (x23, (2,3)), (x24, (2,4)),
           (x30, (3,0)), (x31, (3,1)), (x32, (3,2)), (x33, (3,3)), (x34, (3,4)),
           (x40, (4,0)), (x41, (4,1)), (x42, (4,2)), (x43, (4,3)), (x44, (4,4))]
        in
           readInputsBingoTail (b :: boards, remlns)
        end
      | _ => boards

fun readInputsBingo lns =
   (map Util.intFromStringUnsafe (String.tokens notdigit (hd lns)),
   readInputsBingoTail ([], tl lns))

fun timeScoreTail (t, board, remainingSum, rows, cols, nums) =
   case nums of
        [] => (t, 0)
      | n :: ns =>
            (case IntVectorMap.lookup (board, n) of
                  SOME (i, j) =>
                  let
                     val newRowI = Vector.sub (rows, i) - 1
                     val newColJ = Vector.sub (cols, j) - 1
                     val newSum = remainingSum - n
                  in
                     if newRowI <= 0 orelse newColJ <= 0
                     then (t, newSum * n)
                     else timeScoreTail (
                     t + 1,
                     board,
                     newSum,
                     Vector.update (rows, i, newRowI),
                     Vector.update (cols, j, newColJ),
                     ns)
                  end
                | NONE => timeScoreTail (
                t + 1, board, remainingSum, rows, cols, ns))

fun timeScore (board, nums) = timeScoreTail (
   0,
   board,
   IntVectorMap.foldl (fn (k, _, acc) => k + acc) 0 board,
   Vector.tabulate (5, fn _ => 5),
   Vector.tabulate (5, fn _ => 5),
   nums)
