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

(** Writes a VGPU's pair of GPU_partition references, and the only thing that
    does.

    A call site names the lifecycle [transition] that has happened. What that
    transition means for the two references is [Gpu.Gpu_partition_lifecycle]'s
    answer, not the site's — which is why no function here accepts an
    [outcome]. Six sites used to decide this for themselves; the point of the
    seam is that they can no longer express the decision.

    @group Graphics *)

type chooser =
  __context:Context.t -> self:API.ref_VGPU -> API.ref_GPU_partition option
(** Picks the partition a [Reserve] or [Confirm] should bind the VGPU to.
    Injected so the wiring can ship before the placement policy exists. *)

val choose_none : chooser
(** Picks nothing. The default, and the reason the wiring is inert: with it
    [Set] is unreachable and both references stay null, so behaviour is
    identical to a build without partitions. Placement (V-10) supplies a real
    one. *)

val initial_refs : API.ref_GPU_partition * API.ref_GPU_partition
(** The (resident, scheduled) pair a freshly created VGPU starts with, read
    out of the effect table's [Create] row. A VGPU cannot be born holding a
    partition without that row changing and its mirror test failing. *)

val apply :
     __context:Context.t
  -> self:API.ref_VGPU
  -> ?choose:chooser
  -> Gpu.Gpu_partition_lifecycle.transition
  -> unit
(** [apply ~__context ~self ?choose transition] writes both references
    according to the effect table. Takes the global lock, so the two writes
    are not observable apart. *)

val apply_locked :
     __context:Context.t
  -> self:API.ref_VGPU
  -> ?choose:chooser
  -> Gpu.Gpu_partition_lifecycle.transition
  -> unit
(** As {!apply}, for a caller that already holds the global lock.
    [Helpers.with_global_lock] is a plain mutex and is not re-entrant, so
    calling {!apply} from inside it deadlocks. Exactly one site needs this:
    [VGPU.atomic_set_resident_on]. *)
