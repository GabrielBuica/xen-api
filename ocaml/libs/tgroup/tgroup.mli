module Pthread : sig
  type t
end

module Group : sig
  type t

  module Originator : sig
    type t = Internal_Host_SM | EXTERNAL

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
