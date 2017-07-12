signature ID = sig
  type t
end

functor StackFn(M : ID) : STACK = struct
  type t = M.t
  val store : M.t list ref = ref []

  exception EmptyStack

  fun push x = store := x::(!store)

  fun pop () =
    if List.null (!store)
    then raise EmptyStack
    else
      let
        val r = hd (!store)
      in
        (store := tl (!store); r)
      end

end
