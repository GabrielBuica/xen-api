(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Test_vgpu_common

let assert_true msg b = Alcotest.(check bool) msg true b

let test_supported_enabled_types () =
  let __context = Test_common.make_test_database () in
  (* Create a GPU group containing a single K2 PGPU. *)
  let host = List.hd (Db.Host.get_all ~__context) in
  let gPU_group = Test_common.make_gpu_group ~__context () in
  let pgpu = make_pgpu ~__context ~host ~gPU_group default_k2 in
  (* Update the group's enabled and supported types, and check that they
     	 * contain all the types enabled and supported by the PGPU. *)
  Xapi_gpu_group.update_supported_VGPU_types ~__context ~self:gPU_group ;
  Xapi_gpu_group.update_enabled_VGPU_types ~__context ~self:gPU_group ;
  let vgpu_types_and_refs =
    List.map
      (fun vgpu_type ->
        (vgpu_type, Xapi_vgpu_type.find_or_create ~__context vgpu_type)
      )
      k2_vgpu_types
  in
  let group_supported_types =
    Db.GPU_group.get_supported_VGPU_types ~__context ~self:gPU_group
  in
  let group_enabled_types =
    Db.GPU_group.get_enabled_VGPU_types ~__context ~self:gPU_group
  in
  List.iter
    (fun (vgpu_type, vgpu_type_ref) ->
      let msg_supported =
        Printf.sprintf "GPU group does not list %s as supported"
          vgpu_type.Xapi_vgpu_type.model_name
      in
      let msg_enabled =
        Printf.sprintf "GPU group does not list %s as enabled"
          vgpu_type.Xapi_vgpu_type.model_name
      in
      assert_true msg_supported (List.mem vgpu_type_ref group_supported_types) ;
      assert_true msg_enabled (List.mem vgpu_type_ref group_enabled_types)
    )
    vgpu_types_and_refs ;
  (* Invalidate the PGPU's host ref, and run a GC pass; this should destroy the
     	 * pgpu, and clear the group's supported and enabled types. *)
  Db.PGPU.set_host ~__context ~self:pgpu ~value:Ref.null ;
  Db_gc_util.gc_PGPUs ~__context ;
  let group_supported_types =
    Db.GPU_group.get_supported_VGPU_types ~__context ~self:gPU_group
  in
  let group_enabled_types =
    Db.GPU_group.get_enabled_VGPU_types ~__context ~self:gPU_group
  in
  Alcotest.(check bool)
    "PGPU has not been destroyed" false
    (Db.is_valid_ref __context pgpu) ;
  Alcotest.(check (list (Alcotest_comparators.ref ())))
    "GPU group still has supported types after GC" [] group_supported_types ;
  Alcotest.(check (list (Alcotest_comparators.ref ())))
    "GPU group still has enabled types after GC" [] group_enabled_types

(* One PGPU full of K100s, the other running a K140Q: the group reports the
   type conflict, not "insufficient capacity", whichever PGPU comes first. The
   old scored table ranked capacity above the conflict. *)
let test_remaining_capacity_reports_type_conflict () =
  List.iter
    (fun swap ->
      let __context = Test_common.make_test_database () in
      let host = Helpers.get_localhost ~__context in
      let gPU_group = Test_common.make_gpu_group ~__context () in
      let a = make_pgpu ~__context ~host ~gPU_group default_k1 in
      let b = make_pgpu ~__context ~host ~gPU_group default_k1 in
      let full, conflicting =
        if swap then
          (b, a)
        else
          (a, b)
      in
      let k100 = Xapi_vgpu_type.find_or_create ~__context k100 in
      let rec fill n =
        if n > 64 then Alcotest.fail "the PGPU never filled up" ;
        if
          Xapi_pgpu_helpers.get_remaining_capacity ~__context ~self:full
            ~vgpu_type:k100 ~pre_allocate_list:[]
          > 0L
        then (
          ignore (make_vgpu ~__context ~resident_on:full Test_vgpu_common.k100) ;
          fill (n + 1)
        )
      in
      fill 0 ;
      ignore (make_vgpu ~__context ~resident_on:conflicting k140q) ;
      match
        Xapi_gpu_group.get_remaining_capacity_internal ~__context
          ~self:gPU_group ~vgpu_type:k100
      with
      | Error (Vgpu_refusal.Type_incompatible_with_running {pgpu; _}) ->
          Alcotest.(check string)
            "the conflict reported is the conflicting PGPU's"
            (Ref.string_of conflicting)
            pgpu
      | Error r ->
          Alcotest.fail
            ("expected the type conflict, got " ^ Vgpu_refusal.to_string r)
      | Ok n ->
          Alcotest.failf "expected a refusal, got capacity %Ld" n
    )
    [false; true]

let test =
  [
    ("test_supported_enabled_types", `Quick, test_supported_enabled_types)
  ; ( "test_remaining_capacity_reports_type_conflict"
    , `Quick
    , test_remaining_capacity_reports_type_conflict
    )
  ]
