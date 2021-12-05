fun countIncreasesTail (x1, x2, x3, xs, c) =
   case xs of
        x4 :: ys => countIncreasesTail (x2, x3, x4, ys, if x1 < x4 then c + 1 else c)
      | [] => c

fun countIncreases l =
   case l of
        x1 :: x2 :: x3 :: xs => countIncreasesTail (x1, x2, x3, xs, 0)
      | _ => 0

val () = print (Int.toString (countIncreases (Util.readInputsInt ())) ^ "\n")
