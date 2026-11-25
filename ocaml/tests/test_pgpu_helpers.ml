(*
 * Copyright (C) Citrix Systems Inc.
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

open Test_highlevel
open Test_vgpu_common
open Xapi_vgpu_type

module GetRemainingCapacity = Generic.MakeStateful (struct
  module Io = struct
    type input_t = pgpu_state * vgpu_type

    type output_t = int64

    let string_of_input_t =
      Test_printers.(assoc_pair string_of_pgpu_state string_of_vgpu_type)

    let string_of_output_t = Test_printers.int64
  end

  module State = Test_state.XapiDb

  let load_input __context (pgpu, _) =
    let (_ : API.ref_PGPU) = make_pgpu ~__context pgpu in
    ()

  let extract_output __context (_, vgpu_type) =
    let pgpu_ref = List.hd (Db.PGPU.get_all ~__context) in
    let vgpu_type_ref = find_or_create ~__context vgpu_type in
    Xapi_pgpu_helpers.get_remaining_capacity ~__context ~self:pgpu_ref
      ~vgpu_type:vgpu_type_ref ~pre_allocate_list:[]

  let tests =
    `QuickAndAutoDocumented
      [
        (* Test that empty PGPUs have the correct capacity for each virtual
                * GPU type. *)
        ((default_k1, k100), 8L)
      ; ((default_k1, k140q), 4L)
      ; ((default_k1, passthrough_gpu), 1L)
      ; ((default_k2, k200), 8L)
      ; ((default_k2, k240q), 4L)
      ; ((default_k2, k260q), 2L)
      ; ((default_k2, passthrough_gpu), 1L)
      ; (* Test that we can't mix VGPU types. *)
        (({default_k1 with resident_VGPU_types= [passthrough_gpu]}, k100), 0L)
      ; (({default_k1 with resident_VGPU_types= [k100]}, k140q), 0L)
      ; (({default_k2 with resident_VGPU_types= [passthrough_gpu]}, k200), 0L)
      ; (({default_k2 with resident_VGPU_types= [k260q]}, k200), 0L)
      ; (* Test that remaining capacity values in other situations are correct. *)
        (({default_k1 with resident_VGPU_types= [k100; k100]}, k100), 6L)
      ; (({default_k1 with resident_VGPU_types= [k140q; k140q]}, k140q), 2L)
      ; ( ( {default_k1 with resident_VGPU_types= [passthrough_gpu]}
          , passthrough_gpu
          )
        , 0L
        )
      ; (({default_k2 with resident_VGPU_types= [k200]}, k200), 7L)
      ; ( ({default_k2 with resident_VGPU_types= [k240q; k240q; k240q]}, k240q)
        , 1L
        )
      ; (({default_k2 with resident_VGPU_types= [k260q]}, k260q), 1L)
      ; ( ( {default_k2 with resident_VGPU_types= [passthrough_gpu]}
          , passthrough_gpu
          )
        , 0L
        )
      ; (* Test that scheduled vGPUs also affect the capacity calculations. *)
        (({default_k1 with scheduled_VGPU_types= [k100]}, k100), 7L)
      ; (({default_k1 with scheduled_VGPU_types= [k100]}, passthrough_gpu), 0L)
      ; (({default_k1 with scheduled_VGPU_types= [passthrough_gpu]}, k100), 0L)
      ; ( ( {default_k1 with scheduled_VGPU_types= [passthrough_gpu]}
          , passthrough_gpu
          )
        , 0L
        )
      ; (({default_k2 with scheduled_VGPU_types= [k200; k200]}, k200), 6L)
      ; (({default_k2 with scheduled_VGPU_types= [k200]}, passthrough_gpu), 0L)
      ; (({default_k2 with scheduled_VGPU_types= [passthrough_gpu]}, k200), 0L)
      ; ( ( {default_k2 with scheduled_VGPU_types= [passthrough_gpu]}
          , passthrough_gpu
          )
        , 0L
        )
      ; (* Test that the capacity calculations work with combinations of scheduled
                * and resident VGPUs. *)
        ( ( {
              default_k1 with
              resident_VGPU_types= [k100; k100]
            ; scheduled_VGPU_types= [k100]
            }
          , k100
          )
        , 5L
        )
      ; ( ( {
              default_k2 with
              resident_VGPU_types= [k240q; k240q]
            ; scheduled_VGPU_types= [k240q]
            }
          , k240q
          )
        , 1L
        )
      ]
end)

(* The refusal get_remaining_capacity_internal gives for [vgpu_type] on a
   single PGPU in state [pgpu]. *)
let refusal_for pgpu vgpu_type =
  let __context = Test_common.make_test_database () in
  let pgpu_ref = make_pgpu ~__context pgpu in
  let type_ref = find_or_create ~__context vgpu_type in
  ( type_ref
  , Xapi_pgpu_helpers.get_remaining_capacity_internal ~__context ~self:pgpu_ref
      ~vgpu_type:type_ref ~pre_allocate_list:[]
  )

(* A type whose size is still 0 is refused by name before anything divides
   by it, not as a full PGPU and not as an internal failure. *)
let test_unread_type_refused () =
  match refusal_for k1_with_unread_type unread_type with
  | type_ref, Error (Vgpu_refusal.Type_definition_unavailable {vgpu_type}) ->
      Alcotest.(check string) "type" (Ref.string_of type_ref) vgpu_type
  | _, r ->
      Alcotest.failf "expected Type_definition_unavailable, got %s"
        ( match r with
        | Ok n ->
            Printf.sprintf "capacity %Ld" n
        | Error r ->
            Vgpu_refusal.to_string r
        )

(* The type checks come first: a PGPU that does not support the type says
   so, whatever the type's size. *)
let test_unsupported_unread_type () =
  match refusal_for default_k1 unread_type with
  | _, Error (Vgpu_refusal.Type_not_supported _) ->
      ()
  | _, r ->
      Alcotest.failf "expected Type_not_supported, got %s"
        ( match r with
        | Ok n ->
            Printf.sprintf "capacity %Ld" n
        | Error r ->
            Vgpu_refusal.to_string r
        )

let tests =
  [
    ("test_pgpu_helpers_get_remaining_capacity", GetRemainingCapacity.tests)
  ; ( "test_pgpu_helpers_type_definition_unavailable"
    , [
        ("A size-0 type is refused by name", `Quick, test_unread_type_refused)
      ; ( "An unsupported size-0 type is refused as unsupported"
        , `Quick
        , test_unsupported_unread_type
        )
      ]
    )
  ]
