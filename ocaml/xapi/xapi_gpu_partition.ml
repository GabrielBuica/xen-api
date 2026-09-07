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

module D = Debug.Make (struct let name = "xapi_gpu_partition" end)

open D
module Lifecycle = Gpu.Gpu_partition_lifecycle

type chooser =
  __context:Context.t -> self:API.ref_VGPU -> API.ref_GPU_partition option

let choose_none : chooser = fun ~__context:_ ~self:_ -> None

let constrained ~partition : chooser =
 fun ~__context ~self ->
  let occupants =
    Db.GPU_partition.get_resident_VGPUs ~__context ~self:partition
    |> List.filter (fun vgpu -> vgpu <> self)
  in
  match occupants with
  | [] ->
      Some partition
  | occupant :: _ ->
      let occupying_vm = Db.VGPU.get_VM ~__context ~self:occupant in
      error "%s: VGPU %s is bound to partition %s, occupied by VM %s"
        __FUNCTION__ (Ref.string_of self) (Ref.string_of partition)
        (Ref.string_of occupying_vm) ;
      raise
        (Api_errors.Server_error
           ( Api_errors.gpu_partition_in_use
           , [Ref.string_of partition; Ref.string_of occupying_vm]
           )
        )

let chooser_for ~__context ~self =
  match Db.VGPU.get_resident_on_partition ~__context ~self with
  | partition when partition <> Ref.null ->
      constrained ~partition
  | _ ->
      choose_none

let string_of_action = Lifecycle.string_of_ref_action Ref.string_of

let ref_of_action = function
  | Lifecycle.Set v ->
      v
  | Lifecycle.Clear | Lifecycle.Untouched ->
      Ref.null

let initial_refs =
  let open Lifecycle in
  let eff = outcome_of Create ~chosen:None in
  (ref_of_action eff.resident, ref_of_action eff.scheduled)

let apply_ref_action ~__context ~self ~set action =
  let open Lifecycle in
  match action with
  | Untouched ->
      ()
  | Clear ->
      set ~__context ~self ~value:Ref.null
  | Set value ->
      set ~__context ~self ~value:(value : API.ref_GPU_partition)

let apply_nolock ~__context ~self ?(choose = choose_none) transition =
  let open Lifecycle in
  let chosen = choose ~__context ~self in
  let eff = outcome_of transition ~chosen in
  debug "%s: VGPU %s %s -> resident:%s scheduled:%s" __FUNCTION__
    (Ref.string_of self)
    (string_of_transition transition)
    (string_of_action eff.resident)
    (string_of_action eff.scheduled) ;
  apply_ref_action ~__context ~self ~set:Db.VGPU.set_resident_on_partition
    eff.resident ;
  apply_ref_action ~__context ~self
    ~set:Db.VGPU.set_scheduled_to_be_resident_on_partition eff.scheduled

let apply ~__context ~self ?choose transition =
  Helpers.with_global_lock (fun () ->
      apply_nolock ~__context ~self ?choose transition
  )
