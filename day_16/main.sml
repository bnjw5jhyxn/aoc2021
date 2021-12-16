datatype packet = LITERAL of int * IntInf.int
                | SUM of int * packet list
                | PROD of int * packet list
                | MIN of int * packet * packet list
                | MAX of int * packet * packet list
                | GT of int * packet * packet
                | LT of int * packet * packet
                | EQ of int * packet * packet

fun buildOperatorPack (version, opcode, subpacks) =
   case (opcode, subpacks) of
        (0, _) => SUM (version, subpacks)
      | (1, _) => PROD (version, subpacks)
      | (2, p0 :: subpacks) => MIN (version, p0, subpacks)
      | (3, p0 :: subpacks) => MAX (version, p0, subpacks)
      | (5, [a, b]) => GT (version, a, b)
      | (6, [a, b]) => LT (version, a, b)
      | (7, [a, b]) => EQ (version, a, b)
      | _ => raise Fail "invalid operator arguments"

val bool2int = fn true => 1 | false => 0
fun bl2iTail (l, n) =
   case l of
        b1 :: l1 => bl2iTail (l1, 2 * n + (bool2int b1))
      | [] => n
fun bl2i l = bl2iTail (l, 0)

val str2bits =
   List.concat o
   (map (fn
   #"0" => [false, false, false, false]
 | #"1" => [false, false, false, true]
 | #"2" => [false, false, true, false]
 | #"3" => [false, false, true, true]
 | #"4" => [false, true, false, false]
 | #"5" => [false, true, false, true]
 | #"6" => [false, true, true, false]
 | #"7" => [false, true, true, true]
 | #"8" => [true, false, false, false]
 | #"9" => [true, false, false, true]
 | #"A" => [true, false, true, false]
 | #"B" => [true, false, true, true]
 | #"C" => [true, true, false, false]
 | #"D" => [true, true, false, true]
 | #"E" => [true, true, true, false]
 | #"F" => [true, true, true, true]
 | #"\n" => []
 | _ => raise Fail "illegal character")) o
   explode

fun parseIntTail (l0, bitsConsumed, n) =
   case l0 of
        false :: b1 :: b2 :: b3 :: b4 :: l1 =>
        (l1, bitsConsumed + 5, 16 * n + IntInf.fromInt (bl2i [b1, b2, b3, b4]))
      | true :: b1 :: b2 :: b3 :: b4 :: l1 =>
            parseIntTail (l1, bitsConsumed + 5,
            16 * n + IntInf.fromInt (bl2i [b1, b2, b3, b4]))
      | _ => raise Fail "failed to parse integer"

fun parseInt l0 = parseIntTail (l0, 0, IntInf.fromInt 0)

fun parseSubpacksNumTail (l0, bitsConsumed, n, subpacks) =
   if n <= 0
   then (l0, bitsConsumed, rev subpacks)
   else
      let
         val (l1, packBits, pack) = parsePacket l0
      in
         parseSubpacksNumTail (
         l1, bitsConsumed + packBits, n - 1, pack :: subpacks)
      end
and parseSubpacksBitsTail (l0, bits, subpacks) =
   case Int.compare (bits, 0) of
        LESS => raise Fail "wrong number of bits while parsing subpackets"
      | EQUAL => (l0, rev subpacks)
      | GREATER =>
            let
               val (l1, packBits, pack) = parsePacket l0
            in
               parseSubpacksBitsTail (l1, bits - packBits, pack :: subpacks)
            end
and parsePacket l0 =
   case l0 of
        v1 :: v2 :: v3 :: true :: false :: false :: l1 =>
        let
           val (l2, bitsConsumed, n) = parseInt l1
        in
           (l2, bitsConsumed + 6, LITERAL (bl2i [v1, v2, v3], n))
        end
      | v1 :: v2 :: v3 :: t1 :: t2 :: t3 :: false ::
        b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 ::
        b10 :: b11 :: b12 :: b13 :: b14 :: b15 :: l1 =>
        let
           val bits =
              bl2i [b1, b2, b3, b4, b5, b6, b7, b8,
              b9, b10, b11, b12, b13, b14, b15]
           val (l2, subpacks) =
              parseSubpacksBitsTail (l1, bits, [])
        in
           (l2, bits + 22,
           buildOperatorPack (bl2i [v1, v2, v3], bl2i [t1, t2, t3], subpacks))
        end
      | v1 :: v2 :: v3 :: t1 :: t2 :: t3 :: true ::
        b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 ::
        b10 :: b11 :: l1 =>
        let
           val (l2, bitsConsumed, subpacks) =
              parseSubpacksNumTail (l1, 0,
              bl2i [b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11], [])
        in
           (l2, bitsConsumed + 18,
           buildOperatorPack (bl2i [v1, v2, v3], bl2i [t1, t2, t3], subpacks))
        end
      | _ => raise Fail "wrong number of bits while parsing packet header"

val rec addVersionNumbers =
   fn LITERAL (v, _) => v
      | SUM (v, subpacks) =>
            foldl (fn (p, acc) => acc + (addVersionNumbers p)) v subpacks
      | PROD (v, subpacks) =>
            foldl (fn (p, acc) => acc + (addVersionNumbers p)) v subpacks
      | MIN (v, p0, subpacks) =>
            foldl (fn (p, acc) => acc + (addVersionNumbers p))
            v (p0 :: subpacks)
      | MAX (v, p0, subpacks) =>
            foldl (fn (p, acc) => acc + (addVersionNumbers p))
            v (p0 :: subpacks)
      | GT (v, a, b) => v + (addVersionNumbers a) + (addVersionNumbers b)
      | LT (v, a, b) => v + (addVersionNumbers a) + (addVersionNumbers b)
      | EQ (v, a, b) => v + (addVersionNumbers a) + (addVersionNumbers b)

val rec eval =
   fn LITERAL (_, n) => n
      | SUM (_, subpacks) => foldl (fn (p, acc) => acc + (eval p)) 0 subpacks
      | PROD (_, subpacks) => foldl (fn (p, acc) => acc * (eval p)) 1 subpacks
      | MIN (_, p0, subpacks) =>
            foldl (fn (p, acc) => IntInf.min (acc, (eval p))) (eval p0) subpacks
      | MAX (_, p0, subpacks) =>
            foldl (fn (p, acc) => IntInf.max (acc, (eval p))) (eval p0) subpacks
      | GT (_, a, b) => IntInf.fromInt (bool2int ((eval a) > (eval b)))
      | LT (_, a, b) => IntInf.fromInt (bool2int ((eval a) < (eval b)))
      | EQ (_, a, b) => IntInf.fromInt (bool2int ((eval a) = (eval b)))
val () =
   let
      val (_, _, packet) =
         case Util.readInputs () of
              [ln] => parsePacket (str2bits ln)
            | _ => raise Fail "incorrect number of lines in input"
      val part1 = addVersionNumbers packet
      val () = print (Int.toString part1 ^ "\n")
      val part2 = eval packet
      val () = print (IntInf.toString part2 ^ "\n")
   in
      ()
   end
