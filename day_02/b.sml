fun calculate_displacement_tail (x, d, aim, is) =
   case is of
        FORWARD n :: js => calculate_displacement_tail (x + n, d + aim * n, aim, js)
      | DOWN n :: js => calculate_displacement_tail (x, d, aim + n, js)
      | UP n :: js => calculate_displacement_tail (x, d, aim - n, js)
      | [] => (x, d)

fun calc_prod is =
let
   val (x, d) = calculate_displacement_tail (0, 0, 0, is)
in
   x * d
end

val () = print (Int.toString (calc_prod (read_inputs_inst ())) ^ "\n")
