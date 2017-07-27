structure Solver = struct
  open Utils
  structure A  = Array
  structure L  = List
  structure LP = ListPair
  structure I  = Int

  type system = int list list
  type basis  = int list list
  type stack  = int list * bool list

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

  val solve : system -> basis =
    fn sys => raise Fail "TODO"

end
