structure Tests = struct
  structure S = Solver

  fun runTests (name, args) =
    let
      val test = (S.solve S.example1) = [[4,2,1,0],[0,1,1,1]]
      val result2 =
        [[1, 1, 0, 0],
         [2, 0, 1, 0],
         [1, 0, 2, 1],
         [0, 3, 0, 1],
         [0, 1, 1, 1],
         [0, 0, 3, 2]]
      val test =
        test andalso (S.solve S.example2) = result2
    in
      if test then
        (print "All tests have passed.\n"; 0)
      else
        (print "Some tests failed.\n"; 1)
    end

end
