structure Solver = struct
  open Utils
  structure A  = Array
  structure L  = List
  structure LP = ListPair
  structure I  = Int

  type system = int list list
  type basis  = int list list
  type stack  = int list * bool list
  infix <+>
  val op<+> : int list * int list -> int list =
    fn (xs, ys) => L.map op+ (LP.zip (xs, ys))

  (* Scalar vector multiplication *)
  infix <*>
  val op<*> : int * int list -> int list =
    fn (n, xs) => L.map (fn x => n * x) xs

end
