structure Solver = struct
  open Utils
  structure A = Array

  val vector : int -> 'a list -> 'a A.array =
    fn n => fn elems =>
      A.fromList (List.take (elems, n))

  structure IntArrayOrdered
    :> ORDERED where type t = (int A.array) = struct
    type t = int A.array

    fun compare (a1, a2) =
      let
        fun compare' (n, m) =
          case (n, m) of
            (0, 0) => EQUAL
          | (_, 0) => GREATER
          | (0, _) => LESS
          | (n, m) =>
              (case Int.compare (A.sub (a1, n-1), A.sub (a2, m-1)) of
                 EQUAL => compare' (n-1, m-1)
               | ord => ord)
      in
        compare' (A.length a1, A.length a2)
      end

    fun eq (a1, a2) = EQUAL = compare (a1, a2)
  end

  structure ArraySet = ListSet(structure Elem = IntArrayOrdered)
  type array_set = ArraySet.set

  (*val basis : int -> array_set =
    fn n =>
      let
        val z = vector n (replicate n 0)
      in
        List.app
      end*)

end
