fun atLeastHalf (a, b) = 2 * a >= b
fun lessThanHalf (a, b) = 2 * a < b

fun oxygenTail (i, l) =
   case l of
        x :: [] => boolvec2int x
      | _ => oxygenTail (i + 1,
      if atLeastHalf (Vector.sub (getNumOnes l, i), length l)
      then List.filter (fn v => Vector.sub (v, i)) l
      else List.filter (fn v => not (Vector.sub (v, i))) l)

fun oxygen l = oxygenTail (0, l)

fun scrubberTail (i, l) =
   case l of
        x :: [] => boolvec2int x
      | _ => scrubberTail (i + 1,
      if lessThanHalf (Vector.sub (getNumOnes l, i), length l)
      then List.filter (fn v => Vector.sub (v, i)) l
      else List.filter (fn v => not (Vector.sub (v, i))) l)

fun scrubber l = scrubberTail (0, l)

val () =
   let
      val l = readInputsBoolvec ()
   in
      print ((Int.toString ((oxygen l) * scrubber l)) ^ "\n")
   end
