structure Ps = RedBlackSet (
   structure Elem = PairOrdered (
      structure Ordered1 = IntOrdered
      structure Ordered2 = IntOrdered
   )
)

datatype foldInst = FOLDX of int | FOLDY of int

fun parseFoldInst s =
let
   val n =
      valOf (Int.fromString (
      String.extract (s, String.size "fold along z=", NONE)))
in
   case String.sub (s, String.size "fold along ") of
        #"x" => FOLDX n
      | #"y" => FOLDY n
      | _ => raise Fail "parse fold instruction"
end

fun parseInputTail (lns, pts) =
   case lns of
        [] => raise Fail "unexpected end of file"
      | "\n" :: foldInsts => (pts, map parseFoldInst foldInsts)
      | ln :: remLns =>
            case String.tokens (not o Char.isDigit) ln of
                 [x, y] =>
                 parseInputTail (remLns, Ps.insert pts
                 (valOf (Int.fromString x), valOf (Int.fromString y)))
               | _ => raise Fail "parse point"

fun foldNum (x, n) =
   case Int.compare (x, n) of
        LESS => x
      | EQUAL => raise Fail "reflecting point on fold line"
      | GREATER => 2 * n - x

fun foldx (pts, n) =
   Ps.foldl
   (fn ((x,y), acc) => Ps.insert acc (foldNum (x, n), y))
   Ps.empty
   pts

fun foldy (pts, n) =
   Ps.foldl
   (fn ((x,y), acc) => Ps.insert acc (x, foldNum (y, n)))
   Ps.empty
   pts

fun executeInst (inst, pts) =
   case inst of
        FOLDX n => foldx (pts, n)
      | FOLDY n => foldy (pts, n)

fun printPts pts =
let
   val xMax = Ps.foldl (fn ((x,_), acc) => Int.max (x, acc)) 0 pts
   val yMax = Ps.foldl (fn ((_,y), acc) => Int.max (y, acc)) 0 pts
   fun printRowTail (y, i) =
      if i > xMax
      then print "\n"
      else (print (if Ps.member pts (i, y) then "#" else ".")
         ; printRowTail (y, i + 1))
   fun printMultipleRowsTail j =
      if j > yMax
      then ()
      else (printRowTail (j, 0) ; printMultipleRowsTail (j + 1))
in
   printMultipleRowsTail 0
end

val () =
   let
      val (pts, insts) = parseInputTail (Util.readInputs (), Ps.empty)
      val part1 = Ps.size (executeInst (hd insts, pts))
      val () = print (Int.toString part1 ^ "\n")
      val () = printPts (foldl executeInst pts insts)
   in
      ()
   end
