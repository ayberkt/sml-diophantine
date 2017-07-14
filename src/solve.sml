structure Solver = struct
  open Utils
  structure A = Array
  structure L = List
  structure I = Int
  type 'a vector = 'a A.array

  fun $ (f, x) = f x
  infix 0 $

  val vector : int -> 'a list -> 'a A.array =
    fn n => fn elems => A.fromList (L.take (elems, n))

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

  val toSet : (int vector) list -> array_set
    = L.foldl (fn (x, y) => AS.insert y x) AS.empty

  (* Construct the basis vectors for an n-dimensional space. *)
  val basis : int -> array_set =
    fn n =>
      let
        val zeros  = L.map (fn i => vector i (replicate i 0)) (replicate n n)
        val arrays = mapi (fn (i, xs) => (A.update (xs, i, 1); xs)) zeros
        val result = toSet arrays
        (*val _ = printLn (Int.toString (AS.size result) ^ " many arrays in the set.");*)
        val _ = AS.app prArray result;
      in
        toSet arrays
      end

  val prod : int vector -> int vector -> int =
    fn x => fn y =>
      L.foldl op+ 0 (List.map (fn i => A.sub (x, i) * A.sub (y, i)) (indices x))

  (*val lessEq : int vector -> int vector -> bool =*)
    (*fn x => fn y => IntArrayOrdered.compare (x, y) = LESS*)

  val lessEq : int vector -> int vector -> bool =
    fn x => fn y =>
      L.all (fn i => A.sub (x, i) <= A.sub (y, i)) (indices x)

  (* Remove redundant branches. *)
  val rmRedBrs : array_set -> array_set -> array_set =
    fn a => fn m =>
      let
        val _ = printLn "`rmRedBrs` called"
        val g = fn x => fn y => not (lessEq y x)
        val f = fn x => List.all (g x) (AS.toList m)
      in
         toSet (L.filter f (AS.toList a))
      end

  val bfs : int vector -> int -> array_set -> array_set =
    fn v => fn c => fn a =>
      let
        val _ = printLn "bfs"
        val _ = prArray v
        val f : int vector * array_set -> array_set =
          fn (x, acc) =>
            L.foldl (uncurry o flip $ AS.insert) acc
              (comprehend
                (fn j => (A.update (x, j, A.sub (x, j)+1); x))
                (indices x)
                (fn k => ((prod v x - c) * A.sub (v, k)) < 0))
      in
        AS.foldl f AS.empty a
      end

  fun newMinimalResults (v : int vector) (c : int) (a : array_set) (m : array_set) : int list list =
    if AS.isEmpty a
    then (printLn "nmr case 1"; [])
    else
      let
        val _ = printLn "nmr case 2"
        fun loop m [] =
              let
                val _ = printLn "Gubar."
                val a''  = rmRedBrs (bfs v c a) m
              in newMinimalResults v c a'' m end
          | loop m (x::xs) =
              if (prod v x = c) andalso not (AS.member m x) then
                (printLn "Answer found!!!";
                 arrayToList x::loop (AS.insert m x) xs)
              else
                loop m xs
      in
        loop m (AS.toList a)
      end

  val solve :  int list -> int -> int list list =
    fn v => fn c =>
      let
        val n = L.length v
        val _ = printLn ("[solve] n = " ^ Int.toString n)
        val bases = basis n
        val _ =
          if AS.isEmpty bases then
            printLn "[1] bases is empty"
          else
            printLn "[1] bases is _not_ empty"
        val arg1 = vector n v
        val _ =
          if AS.isEmpty bases then
            printLn "[2] bases is empty"
          else
            printLn "[2] bases is _not_ empty"
      in
        newMinimalResults arg1 c bases AS.empty
      end

  val prSolution = fn s =>
    let
      val showSolution = fn (xss : int list list) =>
        if L.null xss
        then "No solutions"
        else
          (concat o intersperse "; " o
            map (concat o intersperse ", " o map Int.toString)) xss
    in
      printLn ("RESULT: " ^ (showSolution s))
    end

  val _ = prSolution (solve [3, 5, 7, 9] 12)

end
