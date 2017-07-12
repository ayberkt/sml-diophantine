structure Utils = struct

  fun range n =
    if n < 0
    then raise Fail "negative argument to range"
    else
      let
        fun range' 0 = [0]
          | range' n = (range (n-1))@[n]
      in
        range' n
      end

  val replicate : int -> 'a -> 'a list =
    fn n => fn x =>
      let
        fun replicate' 0 = []
          | replicate' n = x::(replicate' (n-1))
      in
          if n >= 0
          then replicate' n
          else raise Fail "replicateStr given negative number"
      end

  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c =
    fn f => fn x => fn y => f (x, y)

  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c) =
    fn f => fn (x, y) => f x y

  val printLn = fn s => s ^ "\n"
end
