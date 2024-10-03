module Pthread : sig
  type t

  val set_name : string -> string option

  val get_name : unit -> string option
end

module Group : sig
  type t

  module Originator : sig
    type t = Internal_Host_SM | EXTERNAL | Internal_Server

    val of_string : string -> t

    val to_string : t -> string
  end

  val of_originator : Originator.t -> t

  val get_originator : t -> Originator.t
end

module Cgroup : sig
  val init : unit -> unit

  val of_originator : Group.Originator.t -> unit
end

module Priority : sig
  val of_originator : Group.Originator.t -> unit
end

type state

val empty_state : state

val of_originator : Group.Originator.t -> unit

val of_req_originator : string option -> unit
