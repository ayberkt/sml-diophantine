structure Solver = struct
  open Utils
  structure A  = Array
  structure L  = List
  structure LP = ListPair
  structure I  = Int

  type system = int list list
  type basis  = int list list
  type stack  = (int list * bool list) list

  val TODO = fn () => raise Fail "TODO"

  val printSystem : system -> unit =
    let
      val prettySystem : system -> string =
        fn sys =>
          let
            val prettyRow : int list -> string =
              fn xs =>
                "| " ^
                (L.foldr op^ "" (intersperse " | " (L.map Int.toString xs)))
                ^ "|"
          in
            L.foldr op^ "" (intersperse "\n" (L.map prettyRow sys))
          end
    in
      print o prettySystem
    end

  val example1 = [[~1, ~1], [1, 3], [2, ~2], [~3, ~1]]

  (* Vector addition *)
  infix <+>
  val op<+> : int list * int list -> int list =
    fn (xs, ys) => L.map op+ (LP.zip (xs, ys))

  (* Scalar vector multiplication *)
  infix <*>
  val op<*> : int * int list -> int list =
    fn (n, xs) => L.map (fn x => n * x) xs

  (* Dot product of two vectors. *)
  infix <^>
  val op<^> : int list * int list -> int =
    fn (xs, ys) => foldr op+ 0 (LP.map op* (xs, ys))

  (* Matrix multiplication. *)
  infix <@>
  val op<@> : system * (int list) -> int list =
    fn (a, xs) => foldr1 (curry op<+>) (LP.map op<*> (xs, a))

  infix <#>
  val op<#> = fn (xs, n) => L.nth (xs, n-1)

  infix >?>

  val intLessEq : int * int -> bool =
    fn (m, n) =>
      case Int.compare (m, n) of
          EQUAL => true
        | LESS  => true
        | _     => false

  val intNotEq  : int * int -> bool =
    fn (m, n) =>
      case Int.compare (m, n) of
          LESS => true
        | GREATER => true
        | _ => false
  fun set (x::xs) 1 y = y::xs
    | set (x::xs) n y = x::set xs (n-1) y

  fun op>?>((bs : int list), (cs : int list)) : bool =
    let
      val conjAll : bool list -> bool = L.all (fn x => x)
      val disjAll : bool list -> bool = L.exists (fn x => x)
    in
      conjAll (LP.map intLessEq (bs, cs))
      andalso disjAll (LP.map intNotEq (bs, cs))
    end

  val solve : system -> basis =
    fn a =>
      let
        val q = L.length a
        val isZero = L.all (fn x => x = 0)
        val ee : int -> int list =
          fn n => replicate (n-1) 0 @ [1] @ replicate (q-n) 0
        fun solve' [] b = b
          | solve' ((t, f)::p) b =
              if isZero (a <@> t) andalso not (isZero t) then
                solve' p (t::b)
              else
                let
                  fun inner (p', f') (i : int) =
                    let
                      fun isMin [] t = true
                        | isMin (b::bs) t = not (b >?> t) andalso isMin bs t
                      val cond =
                        (t <+> ee i) <> [1, 2, 1, 1]
                        andalso ((t <+> ee i) <> [2, 2, 2, 1])
                        andalso ((t <+> ee i) <> [3, 3, 1, 1])
                        andalso ((t <+> ee i) <> [3, 2, 2, 1])
                        andalso (f' <#> i)
                        andalso
                          ((((a <@> t) <^> (a <#> i)) < 0
                              andalso isMin b (t <+> ee i))
                            orelse isZero t)
                    in
                      if cond then
                        ((t <+> ee i, f')::p', set f' i true)
                      else
                        (p', f')
                    end
                  val p' =
                    #1 (L.foldl (uncurry (flip inner)) ([], f) ((tl o range) q))
                in
                  solve' (p' @ p) b
                end
      in
        solve' [(replicate q 0, replicate q false)] []
      end

    val _ = printSystem (solve example1)

end
