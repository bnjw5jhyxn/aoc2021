fun count_increases_tail (x1, x2, x3, xs, c) =
   case xs of
        x4 :: ys => count_increases_tail (x2, x3, x4, ys, if x1 < x4 then c + 1 else c)
      | [] => c

fun count_increases l =
   case l of
        x1 :: x2 :: x3 :: xs => count_increases_tail (x1, x2, x3, xs, 0)
      | _ => 0

val () = print (Int.toString (count_increases (Util.read_inputs_int ())) ^ "\n")
