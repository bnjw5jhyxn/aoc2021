fun boolvec_from_string s = Vector.tabulate (String.size s - 1,
   fn i => String.sub (s, i) = #"1")

fun read_inputs_boolvec () = map boolvec_from_string (Util.read_inputs ())

fun bool2int b = if b then 1 else 0

fun boolvec2int_tail (i, n, v) =
   if i >= Vector.length v
   then n
   else boolvec2int_tail (i + 1, 2 * n + bool2int (Vector.sub (v, i)), v)

fun boolvec2int v = boolvec2int_tail (0, 0, v)

fun get_num_ones_tail (v, ss) =
   case ss of
        s :: ts => get_num_ones_tail (Vector.tabulate (Vector.length v,
        fn i => Vector.sub (v, i) + bool2int (Vector.sub (s, i))), ts)
      | [] => v

fun get_num_ones ss =
   case ss of
        s :: ts => get_num_ones_tail (Vector.map bool2int s, ts)
      | [] => Vector.tabulate (0, fn _ => 0)
