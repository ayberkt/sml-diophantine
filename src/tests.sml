structure Tests = struct
  structure S = Solver
  structure T = Testing

  val result2 =
    [[1, 1, 0, 0],
     [2, 0, 1, 0],
     [1, 0, 2, 1],
     [0, 3, 0, 1],
     [0, 1, 1, 1],
     [0, 0, 3, 2]]

  val cases =
    [
      ("Example 1", (S.solve S.example1, [[4, 2, 1, 0], [0, 1, 1, 1]]))
    , ("Example 2", (S.solve S.example2, result2))
    , ("Example 3", (S.solve S.example3, [[1, 10]]))
    ]

  fun runTests (name, args) = T.runTests cases

end
