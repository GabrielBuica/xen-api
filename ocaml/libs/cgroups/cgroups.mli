module Group : sig
  type t

  val of_originator : string -> t

  val init : unit -> unit

  val attach_task : t -> string -> unit
end

type state

val empty_state : state

val set_cur_cgroup : originator:string -> unit
