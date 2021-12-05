fun calculateDisplacementTail (x, d, aim, is) =
   case is of
        FORWARD n :: js => calculateDisplacementTail (x + n, d + aim * n, aim, js)
      | DOWN n :: js => calculateDisplacementTail (x, d, aim + n, js)
      | UP n :: js => calculateDisplacementTail (x, d, aim - n, js)
      | [] => (x, d)

fun calcProd is =
let
   val (x, d) = calculateDisplacementTail (0, 0, 0, is)
in
   x * d
end

val () = print (Int.toString (calcProd (readInputsInst ())) ^ "\n")
