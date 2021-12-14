structure Id = IntRedBlackDict
structure Is = IntRedBlackSet

fun int2char i = String.sub ("BCFHKNOPSV", i)

val char2int =
   fn #"B" => 0
  | #"C" => 1
  | #"F" => 2
  | #"H" => 3
  | #"K" => 4
  | #"N" => 5
  | #"O" => 6
  | #"P" => 7
  | #"S" => 8
  | #"V" => 9
  | _ => raise Fail "invalid character"

val numChars = 10

fun charPair2int (c1, c2) = numChars * (char2int c1) + (char2int c2)

fun string2vec s =
let
   val (c1, cs) =
      case explode (String.substring (s, 0, String.size s - 1)) of
           x :: xs => (x, xs)
         | [] => raise Fail "empty string"
   fun makeSparseVecTail (c1, cs, d) =
      case cs of
           c2 :: remCs => makeSparseVecTail (c2, remCs,
           Id.insertMerge d (charPair2int (c1, c2)) (IntInf.fromInt 1)
           (fn i => i + 1))
         | [] => Vector.tabulate (numChars * numChars,
            fn i => getOpt (Id.find d i, 0))
in
   makeSparseVecTail (c1, cs, Id.empty)
end

fun processRule (ln, d) =
let
   val a = String.sub (ln, 0)
   val c = String.sub (ln, 1)
   val b = String.sub (ln, 6)
   val ac = charPair2int (a, c)
   val ab = charPair2int (a, b)
   val bc = charPair2int (b, c)
in
   Id.insert d ac (Is.insert (Is.singleton ab) bc)
end

fun identityDictTail (n, d) =
   if n < 0
   then d
   else identityDictTail (n - 1, Id.insert d n (Is.singleton n))

val parseInput =
   fn initStr :: "\n" :: rules =>
   let
      val matrixCols =
         foldl processRule
         (identityDictTail (numChars * numChars - 1, Id.empty))
         rules
   in
      (char2int (String.sub (initStr, 0)),
      char2int (String.sub (initStr, String.size initStr - 2)),
      string2vec initStr,
      IntMatrix.tabulate (numChars * numChars,
      fn (i,j) => if Is.member (Id.lookup matrixCols j) i
                  then IntInf.fromInt 1
                  else IntInf.fromInt 0))
   end
  | _ => raise Fail "wrong input format"

fun countChars (vec, c1, cn) =
let
   val double =
      Vector.foldli
      (fn (i, count, acc) =>
      let
         val c1 = i div numChars
         val c2 = i mod numChars
      in
         Vector.tabulate (numChars, fn j =>
         if j = c1 andalso j = c2
         then Vector.sub (acc, j) + 2 * count
         else if j = c1 orelse j = c2
         then Vector.sub (acc, j) + count
         else Vector.sub (acc, j))
      end)
      (Vector.tabulate (numChars, fn i =>
      if i = c1 andalso i = cn
      then IntInf.fromInt 2
      else if i = c1 orelse i = cn
      then IntInf.fromInt 1
      else IntInf.fromInt 0))
      vec
in
   Vector.map (fn x => x div 2) double
end

val maxVec = Vector.foldl IntInf.max 0
val minVec = Vector.foldl (fn (x, acc) => if acc = 0 then x
                                       else if x = 0 then acc
                                       else IntInf.min (x, acc)) 0

val () =
   let
      val (c1, cn, v0, mat) = parseInput (Util.readInputs ())
      val count10 = countChars (IntMatrix.powMulVec (mat, 10, v0), c1, cn)
      val () =
         print (IntInf.toString (maxVec count10 - minVec count10) ^ "\n")
      val count40 = countChars (IntMatrix.powMulVec (mat, 40, v0), c1, cn)
      val () =
         print (IntInf.toString (maxVec count40 - minVec count40) ^ "\n")
   in
      ()
   end
