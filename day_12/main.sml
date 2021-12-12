structure Sd = StringRedBlackDict
structure Ss = StringRedBlackSet

fun isBig s = Char.isUpper (String.sub (s, 0))

fun readEdge s =
   case String.tokens (not o Char.isAlpha) s of
        [v1, v2] => (v1, v2)
      | _ => raise Fail ("failed to parse line ^ s")

val readGraph =
   foldl
   (fn (ln, acc) =>
   let
      val (v1, v2) = readEdge ln
      val acc1 =
         Sd.insertMerge acc v1 (Ss.singleton v2) (fn s => Ss.insert s v2)
   in
         Sd.insertMerge acc1 v2 (Ss.singleton v1) (fn s => Ss.insert s v1)
   end)
   Sd.empty

fun countPaths1Rec (g, v, blocked) =
   if v = "end"
   then 1
   else
      Ss.foldl
      (fn (v2, acc) =>
      if Ss.member blocked v2
      then acc
      else
         acc + countPaths1Rec (g, v2,
         if isBig v then blocked else Ss.insert blocked v))
      0
      (Sd.lookup g v)

fun countPaths2Rec (g, v, blocked, smallVisit) =
   if v = "end"
   then 1
   else
      Ss.foldl
      (fn (v2, acc) =>
      if (Ss.member blocked v2 andalso smallVisit) orelse v2 = "start"
      then acc
      else
         acc + countPaths2Rec (
         g, v2,
         if isBig v then blocked else Ss.insert blocked v,
         smallVisit orelse Ss.member blocked v2))
      0
      (Sd.lookup g v)

val () =
   let
      val g = readGraph (Util.readInputs ())
      val part1 = countPaths1Rec (g, "start", Ss.empty)
      val () = print (Int.toString part1 ^ "\n")
      val part2 = countPaths2Rec (g, "start", Ss.empty, false)
      val () = print (Int.toString part2 ^ "\n")
   in
      ()
   end
