fun gamma l = boolvec2int (Vector.map
   (fn x => x > length l div 2) (getNumOnes l))

fun epsilon l = boolvec2int (Vector.map
   (fn x => x <= length l div 2) (getNumOnes l))

val () =
   let
      val l = readInputsBoolvec ()
   in
      print ((Int.toString ((gamma l) * (epsilon l))) ^ "\n")
   end
