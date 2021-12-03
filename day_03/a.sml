fun gamma l = boolvec2int (Vector.map
   (fn x => x > length l div 2) (get_num_ones l))

fun epsilon l = boolvec2int (Vector.map
   (fn x => x <= length l div 2) (get_num_ones l))

val () =
   let
      val l = read_inputs_boolvec ()
   in
      print ((Int.toString ((gamma l) * (epsilon l))) ^ "\n")
   end
