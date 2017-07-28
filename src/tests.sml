structure Tests = struct
  structure S = Solver

  fun runTests (name, args) =
    let
      val test = (S.solve S.example1) = [[4,2,1,0],[0,1,1,1]]
    in
      if test then
        (print "All tests have passed.\n"; 0)
      else
        (print "Some tests failed.\n"; 1)
    end

end
