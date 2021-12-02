structure Util :> UTIL = struct

fun read_inputs_tail l =
   case TextIO.inputLine TextIO.stdIn of
        SOME ln => read_inputs_tail (ln :: l)
      | NONE => l

fun read_inputs () = rev (read_inputs_tail [])

fun read_inputs_int () = List.mapPartial Int.fromString (read_inputs ())

end
