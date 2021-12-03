fun at_least_half (a, b) = 2 * a >= b
fun less_than_half (a, b) = 2 * a < b

fun oxygen_tail (i, l) =
   case l of
        x :: [] => boolvec2int x
      | _ => oxygen_tail (i + 1,
      if at_least_half (Vector.sub (get_num_ones l, i), length l)
      then List.filter (fn v => Vector.sub (v, i)) l
      else List.filter (fn v => not (Vector.sub (v, i))) l)

fun oxygen l = oxygen_tail (0, l)

fun scrubber_tail (i, l) =
   case l of
        x :: [] => boolvec2int x
      | _ => scrubber_tail (i + 1,
      if less_than_half (Vector.sub (get_num_ones l, i), length l)
      then List.filter (fn v => Vector.sub (v, i)) l
      else List.filter (fn v => not (Vector.sub (v, i))) l)

fun scrubber l = scrubber_tail (0, l)

val () =
   let
      val l = read_inputs_boolvec ()
   in
      print ((Int.toString ((oxygen l) * scrubber l)) ^ "\n")
   end
