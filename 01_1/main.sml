fun count_increases_tail (x, xs, c) =
   case xs of
        y :: ys => count_increases_tail (y, ys, if x < y then c + 1 else c)
      | [] => c

fun count_increases l =
   case l of
        x :: xs => count_increases_tail (x, xs, 0)
      | [] => 0

val () = print (Int.toString (count_increases (Util.read_inputs_int ())) ^ "\n")
