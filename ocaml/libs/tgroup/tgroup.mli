module Group : sig
  type t

  val of_originator : string -> t

  
end

module Cgroup : sig
  val init : unit -> unit

  val attach_task : Group.t -> string -> unit

  val set_cur_cgroup : originator:string -> unit
end

type state

val empty_state : state


