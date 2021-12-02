datatype inst = FORWARD of int | DOWN of int | UP of int

fun inst_from_str s =
   if String.isPrefix "forward " s
   then FORWARD (valOf (Int.fromString (String.extract
   (s, String.size "forward ", NONE))))
   else if String.isPrefix "down " s
   then DOWN (valOf (Int.fromString (String.extract
   (s, String.size "down ", NONE))))
   else if String.isPrefix "up " s
   then UP (valOf (Int.fromString (String.extract
   (s, String.size "up ", NONE))))
   else raise Match

fun read_inputs_inst () = map inst_from_str (Util.read_inputs ())

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
