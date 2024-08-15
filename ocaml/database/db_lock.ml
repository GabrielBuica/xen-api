(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(* Lock shared between client/slave implementations *)

open Xapi_stdext_pervasives.Pervasiveext

(* Withlock takes dbcache_mutex, and ref-counts to allow the same thread to re-enter without blocking as many times
   as it wants. *)
let dbcache_mutex = Mutex.create ()

let time = ref 0.0

let tid = ref 0

let span_contexts : Tracing.SpanContext.t list ref = ref []

let n = ref 0

let maxtime = ref neg_infinity

let mintime = ref infinity

let thread_reenter_count = ref 0

let allow_thread_through_dbcache_mutex = ref None

let ( let@ ) f x = f x

let with_lock ?span f =
  let me = Thread.id (Thread.self ()) in
  let@ span =
    Tracing.with_child_trace span ~span_links:!span_contexts ~name:"with_lock"
      ~attributes:
        [
          ("xs.db.lock.tid", string_of_int !tid)
        ; ("xs.db.current.tid", string_of_int me)
        ; ( "span.link"
          , match !span_contexts with
            | [] ->
                ""
            | link :: _ ->
                Tracing.SpanContext.to_traceparent link
          )
        ]
  in
  let do_with_lock () =
    let now = Unix.gettimeofday () in
    Mutex.lock dbcache_mutex ;
    let () = tid := me in
    let () =
      span_contexts :=
        Option.fold ~none:[]
          ~some:(fun span -> [Tracing.Span.get_context span])
          span
    in
    let now2 = Unix.gettimeofday () in
    let delta = now2 -. now in
    time := !time +. delta ;
    n := !n + 1 ;
    maxtime := max !maxtime delta ;
    mintime := min !mintime delta ;
    allow_thread_through_dbcache_mutex := Some me ;
    thread_reenter_count := 1 ;
    finally f (fun () ->
        thread_reenter_count := !thread_reenter_count - 1 ;
        if !thread_reenter_count = 0 then (
          allow_thread_through_dbcache_mutex := None ;
          tid := 0 ;
          span_contexts := [] ;
          Mutex.unlock dbcache_mutex
        )
    )
  in
  match !allow_thread_through_dbcache_mutex with
  | None ->
      do_with_lock ()
  | Some id ->
      if id = me then (
        thread_reenter_count := !thread_reenter_count + 1 ;
        finally f (fun () -> thread_reenter_count := !thread_reenter_count - 1)
      ) else
        do_with_lock ()

(* Global flush lock: all db flushes are performed holding this lock *)
(* When we want to prevent the database from being flushed for a period
   (e.g. when doing a host backup in the OEM product) then we acquire this lock *)
let global_flush_mutex = Mutex.create ()

let report () = (!n, !time /. float_of_int !n, !mintime, !maxtime)
