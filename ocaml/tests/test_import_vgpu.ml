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

(* Import.assert_can_live_import_vgpu: which failures mean "no GPU here". *)

open Test_vgpu_common

(* A VGPU in [group] of [vgpu_type], on [pgpus] all on the local host, as
   the import sees it. *)
let setup ~pgpus vgpu_type =
  let __context = Test_common.make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let gPU_group = Test_common.make_gpu_group ~__context () in
  let pgpu_refs =
    List.map (fun pgpu -> make_pgpu ~__context ~host ~gPU_group pgpu) pgpus
  in
  let vgpu = make_vgpu ~__context ~gPU_group vgpu_type in
  (__context, pgpu_refs, Db.VGPU.get_record ~__context ~self:vgpu)

let assert_vm_requires_gpu f =
  match f () with
  | () ->
      Alcotest.fail "expected VM_REQUIRES_GPU, but the import was accepted"
  | exception Api_errors.Server_error (code, _)
    when code = Api_errors.vm_requires_gpu ->
      ()

let test_room_on_empty_pgpu () =
  let __context, _, vgpu = setup ~pgpus:[default_k1] k100 in
  Import.assert_can_live_import_vgpu ~__context vgpu

let test_full_pgpu_refused () =
  let full =
    {default_k1 with resident_VGPU_types= List.init 8 (fun _ -> k100)}
  in
  let __context, _, vgpu = setup ~pgpus:[full] k100 in
  assert_vm_requires_gpu (fun () ->
      Import.assert_can_live_import_vgpu ~__context vgpu
  )

(* A refusal from one PGPU must not stop the search: the next one has room. *)
let test_refusal_moves_on () =
  let full =
    {default_k1 with resident_VGPU_types= List.init 8 (fun _ -> k100)}
  in
  let __context, _, vgpu = setup ~pgpus:[full; default_k1] k100 in
  Import.assert_can_live_import_vgpu ~__context vgpu

(* A type whose size is still 0 fits no PGPU, and the import says why
   instead of asking for a GPU on a host with an empty card. *)
let test_unread_type_is_named () =
  let __context, _, vgpu = setup ~pgpus:[k1_with_unread_type] unread_type in
  match Import.assert_can_live_import_vgpu ~__context vgpu with
  | () ->
      Alcotest.fail "expected VGPU_TYPE_DEFINITION_UNAVAILABLE, but accepted"
  | exception Api_errors.Server_error (code, params) ->
      Alcotest.(check string)
        "error code" Api_errors.vgpu_type_definition_unavailable code ;
      Alcotest.(check (list string))
        "parameters"
        [Ref.string_of vgpu.API.vGPU_type]
        params
  | exception Division_by_zero ->
      Alcotest.fail "the size-0 type was divided by"

(* A database failure while deciding is not a refusal. Before the catch-all
   was narrowed this came back as VM_REQUIRES_GPU on an empty card. *)
let test_database_failure_propagates () =
  let __context, pgpu, vgpu =
    match setup ~pgpus:[default_k1] k100 with
    | __context, [pgpu], vgpu ->
        (__context, pgpu, vgpu)
    | _ ->
        Alcotest.fail "expected exactly one PGPU"
  in
  (* An allocated VGPU whose type no longer exists: reading its compatible
     types fails. *)
  let broken = make_vgpu ~__context ~resident_on:pgpu k100 in
  Db.VGPU.set_type ~__context ~self:broken ~value:Ref.null ;
  match Import.assert_can_live_import_vgpu ~__context vgpu with
  | () ->
      Alcotest.fail "expected the database failure, but the import was accepted"
  | exception Api_errors.Server_error (code, _)
    when code = Api_errors.vm_requires_gpu ->
      Alcotest.fail "a database failure was reported as VM_REQUIRES_GPU"
  | exception Db_exn.DBCache_NotFound _ ->
      ()

let test =
  [
    ("Room on an empty PGPU", `Quick, test_room_on_empty_pgpu)
  ; ("A full PGPU is refused", `Quick, test_full_pgpu_refused)
  ; ("A refusal moves on to the next PGPU", `Quick, test_refusal_moves_on)
  ; ("A size-0 type is refused by name", `Quick, test_unread_type_is_named)
  ; ( "A database failure is not reported as no GPU"
    , `Quick
    , test_database_failure_propagates
    )
  ]
