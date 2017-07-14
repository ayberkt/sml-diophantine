structure Solver = struct
  open Utils
  structure A = Array
  structure L = List
  type 'a vector = 'a A.array

  fun $ (f, x) = f x
  infix 0 $

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

  val toSet  : AS.elem list -> array_set =
    L.foldl (fn (x, y) => AS.insert y x) AS.empty

  (* Construct the basis vectors for an n-dimensional space. *)
  val basis : int -> array_set =
    fn n =>
      let
        val zeros  = L.map (fn i => vector i (replicate i 0)) (replicate n n)
        val arrays = L.mapi (fn (i, xs) => (A.update (xs, i, 1); xs)) zeros
        val result = toSet arrays
        (*val _ = printLn (Int.toString (AS.size result) ^ " many arrays in the set.");*)
        val _ = AS.app prArray result;
      in
        toSet arrays
      end

  val prod : int vector -> int vector -> int =
    fn x => fn y =>
      L.foldl op+ 0 (List.map (fn i => A.sub (x, i) * A.sub (y, i)) (indices x))

  val lessEq : int vector -> int vector -> bool =
    fn x => fn y => IntArrayOrdered.compare (x, y) = LESS

  (* Remove redundant branches. *)
  val rmRedBrs : array_set -> array_set -> array_set =
    fn a => fn m =>
      let
        val g : int vector -> int vector -> bool =
          fn x => fn y => not (lessEq y x)
        val f = fn x => AS.all (g x) m
      in
         toSet (L.filter f (AS.toList a))
      end

  val breadthFirstSearch : int vector -> int -> array_set -> array_set =
    fn v => fn c => fn a =>
      let
        val cis =
          fn x =>
            L.filter (fn k => ((prod v x - c) * A.sub (v, k)) < 0) (indices x)
        val f : int vector -> array_set -> array_set = fn x => fn acc =>
          L.foldl (uncurry o flip $ AS.insert) acc
            (L.map (fn j => (A.update (x, j, A.sub (x, j)+1); x)) (cis x))
      in
        AS.foldl (uncurry f) AS.empty a
      end

  fun newMinimalResults (v : int vector) (c : int) (a : array_set) (m : array_set) : int list list =
    if AS.isEmpty m
    then []
    else
      let
        fun loop (m : array_set) [] =
              let
                val a'  = breadthFirstSearch v c a
                val a'' = rmRedBrs a' m
              in
                newMinimalResults v c a'' m
              end
          | loop (m : array_set) ((x : int vector)::xs) =
              if ((prod v x) = c) andalso (AS.all (fn p => p <> x) m)
              then (A.toList x)::(loop (AS.insert m x) xs)
              else loop m xs
      in
        loop m (AS.toList a)
      end

  val solve :  int list -> int -> int list list =
    fn v => fn c =>
      let
        val n = L.length v
      in
        newMinimalResults (vector n v) c (basis n) AS.empty
      end

end
