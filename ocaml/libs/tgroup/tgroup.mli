module Pthread : sig
  type t
end

module Group : sig
  type t

  module Originator : sig
    type t = Internal_Host_SM | EXTERNAL
  end
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
