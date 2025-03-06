(*
 * Copyright (C) 2006-2024 Citrix Systems Inc.
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

module Delay = Xapi_stdext_threads.Threadext.Delay

let delay_wait_check ~min ~max delay timeout expected =
  let cnt = Mtime_clock.counter () in
  let res = Delay.wait delay timeout in
  let elapsed = (Mtime_clock.count cnt |> Mtime.Span.to_float_ns) *. 1e-9 in
  Alcotest.(check bool) "expected result" expected res ;
  if elapsed < min || elapsed > max then
    let msg = Printf.sprintf "%f not in range %f-%f" elapsed min max in
    Alcotest.(check bool) msg true false

(*
Single simple signal stored
- signal
- wait on same thread should succeed quickly
*)
let simple () =
  let d = Delay.make () in
  Delay.signal d ;
  delay_wait_check ~min:0. ~max:0.05 d 1.0 false

(*
No signal
- wait on same thread should timeout more or less on delay
*)
let no_signal () =
  let d = Delay.make () in
  delay_wait_check ~min:0.2 ~max:0.25 d 0.2 true

(*
Signal twice, collapsed
- signal
- signal
- wait on same thread should succeed quickly
- wait on same thread should timeout
*)
let collapsed () =
  let d = Delay.make () in
  Delay.signal d ;
  Delay.signal d ;
  delay_wait_check ~min:0. ~max:0.05 d 0.2 false ;
  delay_wait_check ~min:0.2 ~max:0.25 d 0.2 true

(*
Signal from another thread
- signal on another thread after a while
- wait on same thread should succeed more or less on other thread sleep
*)
let other_thread () =
  let d = Delay.make () in
  let th = Thread.create (fun d -> Thread.delay 0.2 ; Delay.signal d) d in
  delay_wait_check ~min:0.2 ~max:0.25 d 1.0 false ;
  Thread.join th

let tests =
  [
    ("simple", `Quick, simple)
  ; ("no_signal", `Quick, no_signal)
  ; ("collapsed", `Quick, collapsed)
  ; ("other_thread", `Quick, other_thread)
  ]

exception UnitilizedThreadStorage

let failwith_no_storage () = raise UnitilizedThreadStorage

let test_create_ambient_storage () =
  let open Xapi_stdext_threads.Threadext in
  ThreadLocalStorage.create () ;
  let storage =
    match ThreadLocalStorage.get () with
    | Some storage ->
        storage
    | None ->
        failwith_no_storage ()
  in
  let storage_tid = storage.ocaml_tid in
  let ocaml_tid = Thread.self () |> Thread.id in
  Alcotest.(check int)
    "Ocaml thread id matches the thread id stored" ocaml_tid storage_tid

let test_thread_storage_set_and_get () =
  let open Xapi_stdext_threads.Threadext in
  let expected_weight = 1 in
  ThreadLocalStorage.create () ;
  ThreadLocalStorage.set ~weight:expected_weight ;
  match ThreadLocalStorage.get () with
  | Some storage ->
      Alcotest.(check int)
        "Check if correct value is set in storage" expected_weight
        storage.weight
  | None ->
      failwith_no_storage ()

let test_storage_locality () =
  let open Xapi_stdext_threads.Threadext in
  let r1 = ref None in
  let r2 = ref None in

  let thread1_weight = 128 in
  let thread2_weight = 256 in

  let thread1 =
    Thread.create
      (fun () ->
        ThreadLocalStorage.create () ;
        ThreadLocalStorage.set ~weight:thread1_weight ;
        Thread.delay 0.1 ;
        r1 := ThreadLocalStorage.get ()
      )
      ()
  in
  let thread2 =
    Thread.create
      (fun () ->
        ThreadLocalStorage.create () ;
        ThreadLocalStorage.set ~weight:thread2_weight ;
        r2 := ThreadLocalStorage.get ()
      )
      ()
  in
  Thread.join thread1 ;
  Thread.join thread2 ;
  match (!r1, !r2) with
  | Some r1, Some r2 ->
      Alcotest.(check int) "Thread1 weight" thread1_weight r1.weight ;
      Alcotest.(check int) "Thread2 weight" thread2_weight r2.weight
  | _, _ ->
      failwith_no_storage ()

let tls_tests =
  [
    ("create storage", `Quick, test_create_ambient_storage)
  ; ("storage set and get", `Quick, test_thread_storage_set_and_get)
  ; ("thread local storage", `Quick, test_storage_locality)
  ]

let () =
  Alcotest.run "Threadext" [("Delay", tests); ("ThreadLocalStorage", tls_tests)]
