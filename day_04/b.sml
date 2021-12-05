val () =
   let
      val (nums, boards) = readInputsBingo (Util.readInputs ())
      val timeScores = map (fn b => timeScore (b, nums)) boards
      val (_, winScore) = foldl
      (fn ((t1, s1), (t2, s2)) => if t1 >= t2 then (t1, s1) else (t2, s2))
      (hd timeScores) (tl timeScores)
   in
      print (Int.toString winScore ^ "\n")
   end
