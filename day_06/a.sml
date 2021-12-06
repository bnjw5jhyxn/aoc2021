val () =
   let
      val startAges = initAges (readInputsAges ())
      val endAges = simulateDays (startAges, 80)
   in
      print (IntInf.toString (vectorSum endAges) ^ "\n")
   end
