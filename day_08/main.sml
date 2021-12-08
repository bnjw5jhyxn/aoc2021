fun str2light s =
   Vector.fromList (map (fn c => String.isSubstring c s)
   ["a", "b", "c", "d", "e", "f", "g"])

fun readInputLine ln =
   case String.tokens Char.isSpace ln of
        [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, "|", o1, o2, o3, o4] =>
        (map str2light [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9],
        map str2light [o1, o2, o3, o4])
      | _ => raise Fail "parse line"

fun readInputsDigits () = map readInputLine (Util.readInputs ())

val numActiveLights =
   Vector.foldl (fn (b, acc) => if b then acc + 1 else acc) 0

fun is1478 v =
   case numActiveLights v of
        2 => true
      | 4 => true
      | 3 => true
      | 7 => true
      | _ => false

fun countOutput1478 (_, os) =
   foldl (fn (v, acc) => if is1478 v then acc + 1 else acc) 0 os

val bvnot = Vector.map not

fun bvand2 (v1, v2) =
   Vector.tabulate (7, fn i => Vector.sub (v1, i) andalso Vector.sub (v2, i))

fun bvor2 (v1, v2) =
   Vector.tabulate (7, fn i => Vector.sub (v1, i) orelse Vector.sub (v2, i))

fun bvminus (v1, v2) =
   Vector.tabulate (7, fn i =>
   Vector.sub (v1, i) andalso not (Vector.sub (v2, i)))

fun bvand3 (v1, v2, v3) =
   Vector.tabulate (7, fn i =>
   Vector.sub (v1, i) andalso Vector.sub (v2, i) andalso Vector.sub (v3, i))

val bvor = foldl bvor2 (Vector.tabulate (7, fn _ => false))

val oneOf =
   fn (true, false, false) => true
  | (false, true, false) => true
  | (false, false, true) => true
  | _ => false

val twoOf =
   fn (true, true, false) => true
  | (true, false, true) => true
  | (false, true, true) => true
  | _ => false

fun bvOneOf (v1, v2, v3) = Vector.tabulate (7, fn i =>
   oneOf (Vector.sub (v1, i), Vector.sub (v2, i), Vector.sub (v3, i)))

fun bvTwoOf (v1, v2, v3) = Vector.tabulate (7, fn i =>
   twoOf (Vector.sub (v1, i), Vector.sub (v2, i), Vector.sub (v3, i)))

fun isInTuple (v, (v1, v2, v3)) = v = v1 orelse v = v2 orelse v = v3

fun assert (b, s) = if b then () else raise Fail s

fun decipher l =
let
   val len2 =
      case List.filter (fn v => numActiveLights v = 2) l of
           [x] => x
         | _ => raise Fail "not exactly one digit with 2 lights"
   val len3 =
      case List.filter (fn v => numActiveLights v = 3) l of
           [x] => x
         | _ => raise Fail "not exactly one digit with 3 lights"
   val len4 =
      case List.filter (fn v => numActiveLights v = 4) l of
           [x] => x
         | _ => raise Fail "not exactly one digit with 4 lights"
   val len5 =
      case List.filter (fn v => numActiveLights v = 5) l of
           [x1, x2, x3] => (x1, x2, x3)
         | _ => raise Fail "not exactly three digits with 5 lights"
   val len6 =
      case List.filter (fn v => numActiveLights v = 6) l of
           [x1, x2, x3] => (x1, x2, x3)
         | _ => raise Fail "not exactly three digits with 6 lights"
   val len7 =
      case List.filter (fn v => numActiveLights v = 7) l of
           [x] => x
         | _ => raise Fail "not exactly one digit with 7 lights"
   val cf = len2
   val acf = len3
   val be = bvOneOf len5
   val adg = bvand3 len5
   val abfg = bvand3 len6
   val bf = bvminus (abfg, adg)
   val a = bvminus (acf, cf)
   val dg = bvminus (adg, a)
   val b = bvminus (bf, cf)
   val c = bvminus (cf, bf)
   val d = bvminus (adg, abfg)
   val e = bvminus (be, b)
   val f = bvminus (bf, b)
   val g = bvminus (dg, d)
   val () = assert (numActiveLights a = 1, "a does not have one light")
   val () = assert (numActiveLights b = 1, "b does not have one light")
   val () = assert (numActiveLights c = 1, "c does not have one light")
   val () = assert (numActiveLights d = 1, "d does not have one light")
   val () = assert (numActiveLights e = 1, "e does not have one light")
   val () = assert (numActiveLights f = 1, "f does not have one light")
   val () = assert (numActiveLights g = 1, "g does not have one light")
   val zero = bvor [a, b, c, e, f, g]
   val one = bvor [c, f]
   val two = bvor [a, c, d, e, g]
   val three = bvor [a, c, d, f, g]
   val four = bvor [b, c, d, f]
   val five = bvor [a, b, d, f, g]
   val six = bvor [a, b, d, e, f, g]
   val seven = bvor [a, c, f]
   val eight = bvor [a, b, c, d, e, f, g]
   val nine = bvor [a, b, c, d, f, g]
   val () = assert (isInTuple (zero, len6), "zero not among inputs")
   val () = assert (one = len2, "one not among inputs")
   val () = assert (isInTuple (two, len5), "two not among inputs")
   val () = assert (isInTuple (three, len5), "three not among inputs")
   val () = assert (four = len4, "one not among inputs")
   val () = assert (isInTuple (five, len5), "five not among inputs")
   val () = assert (isInTuple (six, len6), "six not among inputs")
   val () = assert (seven = len3, "one not among inputs")
   val () = assert (eight = len7, "one not among inputs")
   val () = assert (isInTuple (nine, len6), "nine not among inputs")
in
   Vector.fromList [
   zero, one, two, three, four, five, six, seven, eight, nine]
end

fun vec2digitTail (v, digits, i) =
   if v = Vector.sub (digits, i) then i else vec2digitTail (v, digits, i + 1)

fun vec2digit (v, digits) = vec2digitTail (v, digits, 0)

fun decodeOutput (d, outputs) =
let
   val digits = decipher d
in
   case outputs of
        [o1, o2, o3, o4] => 1000 * vec2digit (o1, digits)
        + 100 * vec2digit (o2, digits)
        + 10 * vec2digit (o3, digits)
        + vec2digit (o4, digits)
      | _ => raise Fail "output is not four digits"
end

val () =
   let
      val input = readInputsDigits ()
      val part1 = foldl (op +) 0 (map countOutput1478 input)
      val () = print (Int.toString part1 ^ "\n")
      val part2 = foldl (op +) 0 (map decodeOutput input)
      val () = print (Int.toString part2 ^ "\n")
   in
      ()
   end
