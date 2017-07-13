structure Solver = struct
  open Utils
  structure A = Array
  structure L = List
  type 'a array = 'a A.array

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

  structure AS = ListSet(structure Elem = IntArrayOrdered)
  type array_set = AS.set

  fun prArray ar =
    let
      fun prArray' () =
        Array.appi (fn (i, x) => (print (Int.toString x ^ " | "); ())) ar
    in
      (prArray' (); print "\n")
    end

  (* Construct the basis vectors for an n-dimensional space. *)
  val basis : int -> array_set =
    fn n =>
      let
        val zeros  = L.map (fn i => vector i (replicate i 0)) (replicate n n)
        val arrays = L.mapi (fn (i, xs) => (A.update (xs, i, 1); xs)) zeros
        val toSet  = L.foldl (fn (x, y) => AS.insert y x) AS.empty
        val result = toSet arrays
        (*val _ = printLn (Int.toString (AS.size result) ^ " many arrays in the set.");*)
        val _ = AS.app prArray result;
      in
        toSet arrays
      end

  val newMinimalResults : int array -> int -> array_set -> array_set -> int list list =
    fn ar => fn n => fn ars1 => fn ars2 => raise Fail "TODO"

  val breadthFirstSearch : int array -> int -> array_set -> array_set =
    fn ar => fn n => fn ars => raise Fail "TODO"

  val linDiaphEq :  int list -> int -> int list list =
    fn v => fn c => raise "TODO"

end
