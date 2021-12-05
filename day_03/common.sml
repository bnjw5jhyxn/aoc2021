fun boolvecFromString s = Vector.tabulate (String.size s - 1,
   fn i => String.sub (s, i) = #"1")

fun readInputsBoolvec () = map boolvecFromString (Util.readInputs ())

fun bool2int b = if b then 1 else 0

fun boolvec2intTail (i, n, v) =
   if i >= Vector.length v
   then n
   else boolvec2intTail (i + 1, 2 * n + bool2int (Vector.sub (v, i)), v)

fun boolvec2int v = boolvec2intTail (0, 0, v)

fun getNumOnesTail (v, ss) =
   case ss of
        s :: ts => getNumOnesTail (Vector.tabulate (Vector.length v,
        fn i => Vector.sub (v, i) + bool2int (Vector.sub (s, i))), ts)
      | [] => v

fun getNumOnes ss =
   case ss of
        s :: ts => getNumOnesTail (Vector.map bool2int s, ts)
      | [] => Vector.tabulate (0, fn _ => 0)
