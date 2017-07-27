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
    fn v => Array.foldr op:: [] v

  fun intersperse y [] = []
    | intersperse y [x] = [x]
    | intersperse y (x::xs)=x::y::(intersperse y xs)

  fun comprehend (f : 'a -> 'b) (xs : 'a list) (p : 'a -> bool) =
    List.map f (List.filter p xs)

  fun foldr1 (f : 'a -> 'a -> 'a) (xs : 'a list) : 'a =
    let
      val mf : 'a * 'a option -> 'a option =
        fn (x, m) =>
          SOME
            (case m of
               NONE => x
             | SOME y => f x y)
    in
      case List.foldr mf NONE xs of
          SOME a => a
        | NONE => raise Fail "foldr1 got empty list"
    end

end
