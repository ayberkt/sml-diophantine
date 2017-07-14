structure Utils = struct
  structure A = Array

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

  val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c) =
    fn f => fn x => fn y => f y x

  val printLn = fn s => print (s ^ "\n")

  val sum = List.foldl op+ 0

  val indices = fn ar => (range o flip (curry op-) 1 o A.length) ar

  val mapi =
    fn f => fn (xs : 'a list) =>
      let
        fun mapi' f [] _ = []
          | mapi' f (x::xs) n = (f (n, x))::(mapi' f xs (n+1))
      in mapi' f xs 0 end

  val arrayToList : 'a Array.array -> 'a list =
    fn v => Array.foldl op:: [] v

end
