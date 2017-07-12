structure Utils = struct

  fun range n =
    if n < 0
    then raise Fail "negative argument to range"
    else
      let
        fun range' 0 = [0]
          | range' n = n::range (n-1)
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

end
