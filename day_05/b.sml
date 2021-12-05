fun processLine (ln, d) =
   case castHorizontalLine ln of
        SOME (x1, x2, y) => addHorizontalLine (d, x1, x2, y)
      | None => (case castVerticalLine ln of
                      SOME (x, y1, y2) => addVerticalLine (d, x, y1, y2)
                    | NONE =>
                          let
                             val (x1, y1, x2, y2) = ln
                          in
                             addDiagonalLine (d, x1, y1, x2, y2)
                          end)

val () =
   let
      val pointCounts = foldl processLine IntPairDict.empty (readInputsLines ())
      val numMultiple = IntPairDict.foldl
      (fn (_, n, acc) => if n >= 2 then acc + 1 else acc)
      0 pointCounts
   in
      print (Int.toString numMultiple ^ "\n")
   end
