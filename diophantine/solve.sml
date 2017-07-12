structure Utils = struct
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

structure Solver = struct
  open Utils

  structure L = List

  type elt = {
    frozen : bool,
    value : int
  }

  type node = {
    tuple : elt list,
    default : int list,
    constr : int list list
  }

  val solutions : int list list = []

  structure NodeStack = StackFn(struct type t = node end)

end
