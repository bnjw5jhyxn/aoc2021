fun readInputsAges () =
   case Util.readInputs () of
        [ln] => map (valOf o Int.fromString)
        (String.tokens (not o Char.isDigit) ln)
      | _ => raise Fail "parse input"

val initAges =
   foldl (fn (age, vec) => Vector.update (vec, age, Vector.sub (vec, age) + 1))
   (Vector.tabulate (9, fn _ => IntInf.fromInt 0))

fun simulateDay vec =
   Vector.tabulate (9, fn
   8 => Vector.sub (vec, 0)
  | 6 => IntInf.+ (Vector.sub (vec, 7), Vector.sub (vec, 0))
  | i => Vector.sub (vec, i + 1))

fun simulateDays (vec, i) =
   if i <= 0
   then vec
   else simulateDays (simulateDay vec, i - 1)

val vectorSum = Vector.foldl IntInf.+ 0
