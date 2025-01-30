(*
 * Copyright (C) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

val set : ?sampling_rate:float -> float -> unit
(** [set ?sampling_rate interval] calls [Thread.yield ()] at most [interval] seconds.

  The implementation of [Thread.yield] guarantees since OCaml 4.09 that we'll switch to a different OCaml thread,
  if one exists that is not blocked (i.e. it doesn't rely on [sched_yield] which may run the same thread again,
  but uses pthread mutexes and condition variables to ensure the current thread isn't immediately runnable).

  The setting is global for the entire process, and currently uses [Gc.Memprof] to ensure that a hook function is called periodically,
  although it depends on the allocation rate of the program whether it gets called at all.

  Another alternative would be to use {!val:Unix.set_itimer}, but XAPI doesn't cope with [EINTR] in a lot of places,
  and POSIX interval timers rely on signals to notify of elapsed time.

  We could also have a dedicated thread that sleeps for a certain amount of time, but if it is an OCaml thread,
  we'd have no guarantees it'd get scheduled often enough (and it couldn't interrupt other threads anyway,
  by the time you'd be running the handler you already gave up running something else).

  It may be desirable to avoid yielding if we are currently holding a lock, see {!val:lock_acquired}, and {!val:lock_released}
  to notify this module when that happens.
*)

val clear : unit -> unit
(** [clear ()] undoes the changes made by [set].
  This is useful for testing multiple timeslices in the same program. *)

val lock_acquired : unit -> unit
(** [lock_acquired ()] notifies about lock acquisition. *)

val lock_released : unit -> unit
(** [lock_acquired ()] notifies about lock release. *)

module Runtime : sig
  (*module Thread_Group : sig
      type tgroup = {
          mutable tgroup_name: string
        ; mutable tgroup_share: int
        ; mutable thread_count: int Atomic.t
        ; mutable time_ideal: Mtime.span
        ; mutable epoch: Mtime.t
      }

      type t = tgroup option

      val create : tgroup_name:string -> tgroup_share:int -> tgroup

      val add : tgroup -> unit

      val with_one_thread_in_tgroup : tgroup -> (unit -> 'a) -> 'a

      val with_one_thread_in_tgroup_opt : t -> (unit -> 'a) -> 'a
    end*)
end
