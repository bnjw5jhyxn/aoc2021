val notdigit = not o Char.isDigit

fun str2line s =
   case map Util.intFromStringUnsafe (String.tokens notdigit s) of
        [a1, a2, a3, a4] => (a1, a2, a3, a4)
      | _ => raise Fail "parse line"

fun readInputsLines () = map str2line (Util.readInputs ())

structure IntPairDict = RedBlackDict (
   structure Key = PairOrdered (
      structure Ordered1 = IntOrdered
      structure Ordered2 = IntOrdered
   )
)

(* List.fold f acc [a, a+1, ..., b] *)
fun rangeFold (f, acc, a, b) =
   if a > b
   then acc
   else rangeFold (f, f (a, acc), a + 1, b)

(* List.foldl f acc [(a0,b0), ..., (an, bn)] *)
fun rangeFoldDouble (f, acc0, a0, an, b0, bn) =
let
   val make_ai = if a0 <= an then fn i => a0 + i else fn i => a0 - i
   val make_bi = if b0 <= bn then fn i => b0 + i else fn i => b0 - i
   val n = abs (an - a0)
in
   rangeFold (fn (i, acc) => f (make_ai i, make_bi i, acc), acc0, 0, n)
end

fun castHorizontalLine (x1, y1, x2, y2) =
   if y1 = y2 then SOME (x1, x2, y1) else NONE

fun addHorizontalLine (d, x1, x2, y) =
let
   val (xlow, xhigh) = if x1 <= x2 then (x1, x2) else (x2, x1)
in
   rangeFold (
   fn (xi, dict) => IntPairDict.insertMerge dict (xi, y) 1 (fn n => n + 1),
   d, xlow, xhigh)
end

fun castVerticalLine (x1, y1, x2, y2) =
   if x1 = x2 then SOME (x1, y1, y2) else NONE

fun addVerticalLine (d, x, y1, y2) =
let
   val (ylow, yhigh) = if y1 <= y2 then (y1, y2) else (y2, y1)
in
   rangeFold (
   fn (yi, dict) => IntPairDict.insertMerge dict (x, yi) 1 (fn n => n + 1),
   d, ylow, yhigh)
end

fun addDiagonalLine (d, x1, y1, x2, y2) =
   rangeFoldDouble (
   fn (xi, yi, dict) => IntPairDict.insertMerge dict (xi, yi) 1 (fn n => n + 1),
   d, x1, x2, y1, y2)
