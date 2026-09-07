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

(* Shares of a card are interchangeable; a partition is not. A VM that
   resumes onto a different partition resumes onto different hardware, so
   suspend leaves the resident reference alone where shutdown clears it. *)

module Lifecycle = Gpu.Gpu_partition_lifecycle

let ref_t () = Alcotest.testable (Fmt.of_to_string Ref.string_of) ( = )

let check_null msg r = Alcotest.check (ref_t ()) msg Ref.null r

let check_is msg expected r = Alcotest.check (ref_t ()) msg expected r

let partition_refs ~__context ~self =
  ( Db.VGPU.get_resident_on_partition ~__context ~self
  , Db.VGPU.get_scheduled_to_be_resident_on_partition ~__context ~self
  )

let card_refs ~__context ~self =
  ( Db.VGPU.get_resident_on ~__context ~self
  , Db.VGPU.get_scheduled_to_be_resident_on ~__context ~self
  )

(* The VGPU needs a real type: force_state_reset recomputes allowed
   operations, which dereferences VGPU.type. *)
let bound_vgpu ~__context ~partition ?(vM = Ref.null) () =
  let _type = Test_common.make_vgpu_type ~__context () in
  Test_common.make_vgpu ~__context ~vM ~_type ~resident_on_partition:partition
    ~scheduled_to_be_resident_on_partition:partition ()

(* -- the divergence, row by row ------------------------------------------- *)

let test_halt_releases_the_partition () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self = bound_vgpu ~__context ~partition () in
  Xapi_gpu_partition.apply ~__context ~self Lifecycle.Release_halted ;
  let resident, scheduled = partition_refs ~__context ~self in
  check_null "halting gives the partition up" resident ;
  check_null "halting clears the reservation too" scheduled

let test_suspend_keeps_the_partition () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self = bound_vgpu ~__context ~partition () in
  Xapi_gpu_partition.apply ~__context ~self Lifecycle.Release_suspended ;
  let resident, scheduled = partition_refs ~__context ~self in
  check_is "a suspended VM keeps its partition" partition resident ;
  check_null "but holds no reservation while it is away" scheduled

let test_checkpoint_clears_nothing () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self = bound_vgpu ~__context ~partition () in
  Xapi_gpu_partition.apply ~__context ~self Lifecycle.Checkpoint ;
  let resident, scheduled = partition_refs ~__context ~self in
  check_is "checkpoint leaves the partition" partition resident ;
  check_is "checkpoint leaves the reservation" partition scheduled

let test_restart_sweep_leaves_the_partition_alone () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self = bound_vgpu ~__context ~partition () in
  Xapi_gpu_partition.apply ~__context ~self Lifecycle.Restart_sweep ;
  let resident, scheduled = partition_refs ~__context ~self in
  check_is
    "the restart sweep must not evict a suspended VM's partition (it clears \
     only what was scheduled)"
    partition resident ;
  check_null "the sweep does clear the reservation" scheduled

(* Of the two release rows, only Release_suspended keeps its partition. The
   whole-table check against whole-card behaviour is
   Test_gpu_partition_lifecycle.test_only_release_suspended_diverges. *)
let test_exactly_one_divergence () =
  let diverging =
    List.filter
      (fun transition ->
        let eff = Lifecycle.outcome_of transition ~chosen:None in
        (* A release that clears the reservation but keeps the resident
           reference is the suspend shape. *)
        eff.Lifecycle.resident = Lifecycle.Untouched
        && eff.Lifecycle.scheduled = Lifecycle.Clear
        &&
        match transition with
        | Lifecycle.Release_halted | Lifecycle.Release_suspended ->
            true
        | _ ->
            false
      )
      Lifecycle.all_transitions
  in
  Alcotest.(check (list string))
    "Release_suspended is the only release that keeps its partition"
    ["Release_suspended"]
    (List.map Lifecycle.string_of_transition diverging)

(* -- the negative test: the headline ---------------------------------------- *)

let test_a_second_vm_is_refused_the_partition () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let sleeping_vm = Test_common.make_vm ~__context ~name_label:"sleeping" () in
  let _sleeping_vgpu = bound_vgpu ~__context ~partition ~vM:sleeping_vm () in
  let newcomer_vm = Test_common.make_vm ~__context ~name_label:"newcomer" () in
  let newcomer = Test_common.make_vgpu ~__context ~vM:newcomer_vm () in
  let choose = Xapi_gpu_partition.constrained ~partition in
  match choose ~__context ~self:newcomer with
  | Some _ ->
      Alcotest.fail
        "a second VM was allowed onto a partition held by a suspended VM"
  | None ->
      Alcotest.fail "the refusal must be an error, not a silent None"
  | exception Api_errors.Server_error (code, args) ->
      Alcotest.(check string)
        "refused with the partition-specific code"
        Api_errors.gpu_partition_in_use code ;
      Alcotest.(check (list string))
        "the refusal names the partition and the VM occupying it"
        [Ref.string_of partition; Ref.string_of sleeping_vm]
        args

(* The constraint is a constraint, not a blanket refusal. *)
let test_the_sleeper_may_return_to_its_own_partition () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let vm = Test_common.make_vm ~__context ~name_label:"sleeping" () in
  let self = bound_vgpu ~__context ~partition ~vM:vm () in
  let choose = Xapi_gpu_partition.constrained ~partition in
  match choose ~__context ~self with
  | Some p ->
      check_is "the VM resumes onto the partition it kept" partition p
  | None ->
      Alcotest.fail "a VM was refused the partition it is itself holding"

let test_chooser_for_is_inert_for_an_unbound_vgpu () =
  let __context = Test_common.make_test_database () in
  let self = Test_common.make_vgpu ~__context () in
  let choose = Xapi_gpu_partition.chooser_for ~__context ~self in
  Alcotest.(check bool)
    "a VGPU holding no partition still chooses nothing" true
    (choose ~__context ~self = None)

let test_chooser_for_constrains_a_bound_vgpu () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let self = bound_vgpu ~__context ~partition () in
  let choose = Xapi_gpu_partition.chooser_for ~__context ~self in
  match choose ~__context ~self with
  | Some p ->
      check_is "a bound VGPU is constrained to its own partition" partition p
  | None ->
      Alcotest.fail "a bound VGPU must be constrained, not left free"

(* -- card-level accounting is bit-identical --------------------------------- *)

(* A scope guard on the seam: apply writes partition references only. It
   cannot catch a call site that changes its card-level writes. *)
let test_card_level_accounting_is_untouched () =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let pgpu = Test_common.make_pgpu ~__context () in
  List.iter
    (fun transition ->
      let self =
        Test_common.make_vgpu ~__context ~resident_on:pgpu
          ~scheduled_to_be_resident_on:pgpu ~resident_on_partition:partition
          ~scheduled_to_be_resident_on_partition:partition ()
      in
      let before = card_refs ~__context ~self in
      Xapi_gpu_partition.apply ~__context ~self transition ;
      let after = card_refs ~__context ~self in
      let name = Lifecycle.string_of_transition transition in
      Alcotest.(check (pair string string))
        (name ^ ": the PGPU references must not move")
        (Ref.string_of (fst before), Ref.string_of (snd before))
        (Ref.string_of (fst after), Ref.string_of (snd after))
    )
    Lifecycle.all_transitions

(* -- through the real lifecycle path ---------------------------------------- *)

(* force_state_reset is the real site: check the split sends each state to
   the right row. *)
let released_via_force_state_reset ~value =
  let __context = Test_common.make_test_database () in
  let partition = Test_common.make_gpu_partition ~__context () in
  let vm = Test_common.make_vm ~__context () in
  let self = bound_vgpu ~__context ~partition ~vM:vm () in
  Db.VM.set_power_state ~__context ~self:vm ~value ;
  Xapi_vm_lifecycle.force_state_reset ~__context ~self:vm ~value ;
  (partition, partition_refs ~__context ~self)

let test_force_state_reset_halted_releases () =
  let _, (resident, scheduled) =
    released_via_force_state_reset ~value:`Halted
  in
  check_null "force_state_reset to Halted releases the partition" resident ;
  check_null "and the reservation" scheduled

let test_force_state_reset_suspended_keeps () =
  let partition, (resident, scheduled) =
    released_via_force_state_reset ~value:`Suspended
  in
  check_is "force_state_reset to Suspended keeps the partition" partition
    resident ;
  check_null "but drops the reservation" scheduled

let test =
  [
    ("halting releases the partition", `Quick, test_halt_releases_the_partition)
  ; ( "THE DIVERGENCE: suspending keeps it"
    , `Quick
    , test_suspend_keeps_the_partition
    )
  ; ("checkpoint clears nothing", `Quick, test_checkpoint_clears_nothing)
  ; ( "the restart sweep leaves the partition alone"
    , `Quick
    , test_restart_sweep_leaves_the_partition_alone
    )
  ; ("exactly one divergence", `Quick, test_exactly_one_divergence)
  ; ( "NEGATIVE TEST: a second VM is refused the partition"
    , `Quick
    , test_a_second_vm_is_refused_the_partition
    )
  ; ( "the sleeper may return to its own partition"
    , `Quick
    , test_the_sleeper_may_return_to_its_own_partition
    )
  ; ( "chooser_for is inert for an unbound VGPU"
    , `Quick
    , test_chooser_for_is_inert_for_an_unbound_vgpu
    )
  ; ( "chooser_for constrains a bound VGPU"
    , `Quick
    , test_chooser_for_constrains_a_bound_vgpu
    )
  ; ( "card-level accounting is bit-identical"
    , `Quick
    , test_card_level_accounting_is_untouched
    )
  ; ( "force_state_reset Halted releases"
    , `Quick
    , test_force_state_reset_halted_releases
    )
  ; ( "force_state_reset Suspended keeps"
    , `Quick
    , test_force_state_reset_suspended_keeps
    )
  ]
