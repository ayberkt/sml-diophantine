structure Rewriting = struct
  open Matching

  fun rewrite [] t = NONE
    | rewrite ((l, r)::R) t =
        SOME (lift (match (l, t)) r)
        handle NoUnifier => rewrite R t

  fun norm R (V x) = V x
    | norm R (T (f, ts)) =
        let val u = T (f, List.map (norm R) ts)
        in
          case rewrite R u of
            SOME u' => norm R u'
          | NONE => u
        end

  val rules =
    [  (T ("foo", []), T ("bar", []))
    ]

  fun run tm =
    norm [(T ("foo", []), T ("bar", []))] tm

end
