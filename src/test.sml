structure Test = struct
  open Solver
  open Utils

  val v1 = vector 6 [1, 2, 3, 4, 5, 6]
  val v2 = vector 8 [1, 2, 3, 4, 5, 6, 7, 8]
  val v1 = vector 9 [1, 2, 3, 4, 5, 6, 7, 8, 9]

  val X = toSet [A.fromList [1, 2], A.fromList [3, 5]]

  val cases = fn () =>
    [
      ("vector (1)"
      , (IntArrayOrdered.eq (vector 1 [0, 1], Array.fromList [0]), true))
    , ("vector (2)"
      , (IntArrayOrdered.eq (vector 2 [1, 2], Array.fromList [1, 2]), true))
    , ("vector (3)"
      , (IntArrayOrdered.eq (vector 0 [], Array.fromList []), true))
    , ("basis  (1)"
      , (AS.eq
          ( basis 2
          , toSet [A.fromList [0, 1], A.fromList [1, 0]]), true))
    , ("basis  (2)"
      , (AS.eq
          ( basis 1
          , toSet [A.fromList [1]]), true))
    , ("basis  (3)"
      , (AS.eq
          ( basis 0
          , toSet []), true))
    , ("all    (1)"
      , (AS.all (fn _ => true) (toSet [A.array (0, 1), A.array (0, 1)]), true))
    , ("all    (2)"
      , (AS.all (fn _ => false) (toSet [A.array (0, 1), A.array (0, 1)]), false))
    , ("indices (1)"
      , ((indices (A.fromList [0, 1])) = [0, 1], true))
    , ("indices (2)"
      , ((indices (A.fromList [1, 2])) = [0, 1], true))
    , ("indices (3)"
      , ((indices (A.fromList [1])) = [0], true))
    (*, ("bfs"
      , ((breadthFirstSearch v1 5)))*)
    , ("prod"
      , ((prod (vector 5 (range 10)) (vector 10 (range 10))) = 30, true))
    , ("prod"
      , ((prod (vector 19 (range 100)) (vector 32 (range 100))) = 2109, true))
    ]

  val _ = Testing.runTests (cases ())

end
