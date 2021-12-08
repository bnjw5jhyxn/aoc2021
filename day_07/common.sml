fun readInputsPositions () =
   case Util.readInputs () of
        [ln] => map (valOf o Int.fromString)
        (String.tokens (not o Char.isDigit) ln)
      | _ => raise Fail "parse input"

fun median l = List.nth (Util.mergesort Int.compare l, length l div 2)

fun fuelLinear l = foldl (fn (x, acc) => acc + abs (x - median l)) 0 l

fun quadraticCost (start, dest) =
let
   val dist = abs (start - dest)
in
   dist * (dist + 1) div 2
end

fun fuelQuadratic l =
let
   val v = Vector.fromList (Util.mergesort Int.compare l)
   val n = Vector.length v
   val sum = Vector.foldl (op +) 0 v
   fun narrowIndices (i, j) =
      if i + 1 >= j
      then (i, true)
      else
         let
            val mid = (i + j) div 2
            val gradient = n * Vector.sub (v, mid) - sum + (n - mid - 1) - mid
         in
            case Int.compare (gradient, 0) of
                 LESS => narrowIndices (mid, j)
               | EQUAL => (mid, false)
               | GREATER => narrowIndices (i, mid)
         end
   val (idx, isRange) = narrowIndices (0, n - 1)
   val guess = (sum - (n - idx - 1) + (idx + 1)) div n
   val costLow =
      Vector.foldl (fn (x, acc) => acc + quadraticCost (x, guess)) 0 v
   val costHigh =
      Vector.foldl (fn (x, acc) => acc + quadraticCost (x, guess + 1)) 0 v
   val () = if isRange andalso Vector.sub (v, idx) < Vector.sub (v, idx + 1)
   andalso (guess < Vector.sub (v, idx) orelse guess >= Vector.sub (v, idx + 1))
            then raise Fail "gradient bug"
            else ()
in
   if isRange andalso Vector.sub (v, idx) < Vector.sub (v, idx + 1)
   then Int.min (costLow, costHigh)
   else idx
end
