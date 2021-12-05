fun countIncreasesTail (x, xs, c) =
   case xs of
        y :: ys => countIncreasesTail (y, ys, if x < y then c + 1 else c)
      | [] => c

fun countIncreases l =
   case l of
        x :: xs => countIncreasesTail (x, xs, 0)
      | [] => 0

val () = print (Int.toString (countIncreases (Util.readInputsInt ())) ^ "\n")
