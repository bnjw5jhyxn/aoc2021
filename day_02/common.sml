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
