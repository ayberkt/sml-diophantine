structure Rewriting = struct
  open Matching
  infix 3 $

  fun rewrite [] t = NONE
    | rewrite ((l, r)::R) t =
        SOME (lift (match (l, t)) r)
        handle NoUnifier => rewrite R t

  fun norm R (V x) = V x
    | norm R (f $ ts) =
        let val u = f $ List.map (norm R) ts
        in
          case rewrite R u of
            SOME u' => norm R u'
          | NONE => u
        end

  val rules =
    [  ("foo" $ [], "bar" $ [])
    ]

  val run = toString o (norm rules)

end
