val init : unit -> unit

module Creator : sig
  type t

  val default_creator : t

  val name : t -> string

  val of_http_hdr : string option -> t option

  val to_cgroup : t -> string

  val set_priority : t -> unit
end
