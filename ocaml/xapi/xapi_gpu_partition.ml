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

(** The join between the lifecycle effect table and the database.

    [Gpu_partition_lifecycle] decides what a lifecycle event does to a VGPU's
    pair of partition references. This module is the only place that writes
    them. Call sites name a [transition]; they do not decide what it means,
    and they cannot: nothing here takes an [outcome] from outside. *)

module D = Debug.Make (struct let name = "xapi_gpu_partition" end)

open D
module Lifecycle = Gpu.Gpu_partition_lifecycle

type chooser =
  __context:Context.t -> self:API.ref_VGPU -> API.ref_GPU_partition option

(** The default chooser picks nothing, which is what makes the wiring inert:
    with it, [Set] is unreachable and every reference stays null. Placement
    (V-10) replaces it. *)
let choose_none : chooser = fun ~__context:_ ~self:_ -> None

let string_of_ref_action = Lifecycle.string_of_ref_action Ref.string_of

let ref_of_action = function
  | Lifecycle.Set v ->
      v
  | Lifecycle.Clear | Lifecycle.Untouched ->
      Ref.null

(* Read out of the table rather than written as two nulls, so that a VGPU
   born with a reference would require changing the Create row and tripping
   its mirror test — not just editing a constructor call. *)
let initial_refs =
  let eff = Lifecycle.outcome_of Lifecycle.Create ~chosen:None in
  (ref_of_action eff.Lifecycle.resident, ref_of_action eff.Lifecycle.scheduled)

let apply_ref_action ~__context ~self ~set action =
  match action with
  | Lifecycle.Untouched ->
      ()
  | Lifecycle.Clear ->
      set ~__context ~self ~value:Ref.null
  | Lifecycle.Set value ->
      set ~__context ~self ~value:(value : API.ref_GPU_partition)

(* Both references are written under one hold of the global lock: the pair is
   an invariant, and a reader that sees one written and not the other sees a
   VGPU that is both placed and unplaced. See OQ-41. *)
let apply_locked ~__context ~self ?(choose = choose_none) transition =
  let chosen = choose ~__context ~self in
  let eff = Lifecycle.outcome_of transition ~chosen in
  debug "%s: VGPU %s %s -> resident:%s scheduled:%s" __FUNCTION__
    (Ref.string_of self)
    (Lifecycle.string_of_transition transition)
    (string_of_ref_action eff.Lifecycle.resident)
    (string_of_ref_action eff.Lifecycle.scheduled) ;
  apply_ref_action ~__context ~self ~set:Db.VGPU.set_resident_on_partition
    eff.Lifecycle.resident ;
  apply_ref_action ~__context ~self
    ~set:Db.VGPU.set_scheduled_to_be_resident_on_partition
    eff.Lifecycle.scheduled

(* xapi's Helpers.with_global_lock is a plain Mutex.t and is NOT re-entrant,
   so a site already inside it must call apply_locked instead. Exactly one
   does: VGPU.atomic_set_resident_on. *)
let apply ~__context ~self ?choose transition =
  Helpers.with_global_lock (fun () ->
      apply_locked ~__context ~self ?choose transition
  )
