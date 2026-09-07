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

(* SCAFFOLD — this file stands in for CP-314160 (V-01), which owns the
   GPU_partition object model and has not landed. It carries only the parts
   V-03 and V-05 need to compile: the class itself, its PGPU parent, and the
   two reverse sets for the VGPU references.

   Everything V-01 additionally owns — capacity, the profile, partition_mode
   on PGPU, the carvable fields of OQ-02 — is deliberately absent. Drop this
   file and rebase when CP-314160 lands. *)

open Datamodel_types
open Datamodel_common
open Datamodel_roles

let lifecycle = []

let t =
  create_obj ~name:_gpu_partition
    ~descr:
      "A partition of a physical GPU: one of several isolated pieces a single \
       card is carved into, each assignable to a different VM"
    ~doccomments:[] ~gen_constructor_destructor:false ~gen_events:true
    ~in_db:true ~lifecycle ~persist:PersistEverything ~in_oss_since:None
    ~messages:[] ~messages_default_allowed_roles:_R_POOL_OP
    ~contents:
      [
        uid _gpu_partition ~lifecycle
      ; field ~qualifier:StaticRO ~ty:(Ref _pgpu) ~lifecycle "PGPU"
          "The physical GPU this partition is carved from"
          ~default_value:(Some (VRef null_ref))
      ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu)) ~lifecycle
          "resident_VGPUs" "The VGPUs currently running on this partition"
      ; field ~qualifier:DynamicRO ~ty:(Set (Ref _vgpu)) ~lifecycle
          "scheduled_VGPUs" "The VGPUs scheduled to run on this partition"
      ]
    ()
