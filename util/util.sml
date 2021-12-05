structure Util :> UTIL = struct

fun readInputsTail l =
   case TextIO.inputLine TextIO.stdIn of
        SOME ln => readInputsTail (ln :: l)
      | NONE => l

fun readInputs () = rev (readInputsTail [])

val intFromStringUnsafe = valOf o Int.fromString

fun readInputsInt () = map intFromStringUnsafe (readInputs ())

fun mergeTail (cmp, res, l1, l2) =
   case (l1, l2) of
        (_, []) => List.revAppend (res, l1)
      | ([], _) => List.revAppend (res, l2)
      | (x :: xs, y :: ys) =>
            (case cmp (x, y) of
                  LESS => mergeTail (cmp, x :: res, xs, l2)
                | EQUAL => mergeTail (cmp, x :: y :: res, xs, ys)
                | GREATER => mergeTail (cmp, y :: res, l1, ys))

fun merge (cmp, l1, l2) = mergeTail (cmp, [], l1, l2)

fun takeDropTail (i, l1, l2) =
   if i <= 0
   then (l1, l2)
   else takeDropTail (i - 1, hd l2 :: l1, tl l2)

fun takeDrop (i, l) = takeDropTail (i, [], l)

fun mergesort cmp l =
   case l of
        [] => []
      | [x] => l
      | _ =>
            let
               val (l1, l2) = takeDrop (length l div 2, l)
               val l1_sorted = mergesort cmp l1
               val l2_sorted = mergesort cmp l2
            in
               merge (cmp, l1_sorted, l2_sorted)
            end

end
