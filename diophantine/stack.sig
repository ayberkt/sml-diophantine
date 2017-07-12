signature STACK = sig
  type t
  val push  : t -> unit
  val pop   : unit -> t
end
