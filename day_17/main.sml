fun parseInput ln =
   case String.tokens (fn c => not (Char.isDigit c) andalso c <> #"-") ln of
        [xmin, xmax, ymin, ymax] => (
        valOf (Int.fromString xmin),
        valOf (Int.fromString xmax),
        valOf (Int.fromString ymin),
        valOf (Int.fromString ymax))
      | _ => raise Fail "incorrect input format"

fun reachesTarget (x, y, vx, vy, xmin, xmax, ymin, ymax, highest) =
   if xmin <= x andalso x <= xmax andalso ymin <= y andalso y <= ymax
   then (true, highest)
   else if y < ymin
   then (false, highest)
   else reachesTarget (
   x + vx, y + vy,
   if vx >= 1 then vx - 1 else 0, vy - 1,
   xmin, xmax, ymin, ymax,
   Int.max (highest, y))

fun solveTail (vx0, vy0, xmin, xmax, ymin, ymax, highest, numValid) =
   if vx0 > xmax
   then (highest, numValid)
   else if vy0 >= ~ ymin
   then solveTail (
   vx0 + 1, ymin, xmin, xmax, ymin, ymax, highest, numValid)
   else
      let
         val (isValid, height) =
            reachesTarget (0, 0, vx0, vy0, xmin, xmax, ymin, ymax, 0)
      in
         solveTail (
         vx0, vy0 + 1, xmin, xmax, ymin, ymax, Int.max (highest, height),
         if isValid then numValid + 1 else numValid)
      end

val () =
   let
      val (xmin, xmax, ymin, ymax) =
         case Util.readInputs () of
              [ln] => parseInput ln
            | _ => raise Fail "wrong number of lines"
      val (part1, part2) = solveTail (0, ymin, xmin, xmax, ymin, ymax, 0, 0)
      val () = print (Int.toString part1 ^ "\n")
      val () = print (Int.toString part2 ^ "\n")
   in
      ()
   end
