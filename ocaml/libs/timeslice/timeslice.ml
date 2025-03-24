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

module D = Debug.Make (struct let name = "timeslice" end)

(* avoid allocating an extra option every time *)
let invalid_holder = -1

let last_lock_holder = Atomic.make invalid_holder

let me () = Thread.self () |> Thread.id

let lock_acquired () =
  (* these need to be very low overhead, so just keep track of the last lock holder,
     i.e. track only one high-priority lock at a time
  *)
  Atomic.set last_lock_holder (me ())

let lock_released () = Atomic.set last_lock_holder invalid_holder

let[@inline always] am_i_holding_locks () =
  let last = Atomic.get last_lock_holder in
  last <> invalid_holder && last = me ()

let yield_interval = Atomic.make Mtime.Span.zero

(* TODO: use bechamel.monotonic-clock instead, which has lower overhead,
   but not in the right place in xs-opam yet
*)
let last_yield = Atomic.make (Mtime_clock.counter ())

let thread_last_yield = Atomic.make (Mtime_clock.counter ())

let failures = Atomic.make 0

let with_time_counter_now time_counter f =
  let now = Mtime_clock.counter () in
  Atomic.set time_counter now ;
  f ()

module Runtime = struct
  let epoch_count = Atomic.make 0

  let maybe_thread_yield ~global_slice_period =
    let open Xapi_stdext_threads.Threadext in
    let thread_ctx = ThreadLocalStorage.get () in
    thread_ctx.tgroup
    |> Tgroup.ThreadGroup.get_tgroup
    |> Option.iter (fun (tgroup : Tgroup.ThreadGroup.tgroup) ->
           let current_epoch = Atomic.get epoch_count in
           ( if current_epoch <> thread_ctx.tepoch then
               (* thread remembers that it is about to run in a new epoch *)
               let () = thread_ctx.time_running <- Mtime.Span.zero in
               thread_ctx.tepoch <- current_epoch
             else
               (* thread remembers how long it is  running  in the current epoch *)
               let time_running_since_last_yield =
                 Mtime_clock.count (Atomic.get thread_last_yield)
               in
               let time_running =
                 Mtime.Span.add thread_ctx.time_running
                   time_running_since_last_yield
               in
               thread_ctx.time_running <- time_running
           ) ;

           let is_to_sleep_or_yield delay_s =
             if delay_s > 0. then (
               Tgroup.ThreadGroup.with_one_fewer_thread_in_tgroup tgroup
               @@ fun () ->
               D.debug
                 "runtime: sleep=%f s: thread_name=%s time_running=%f s \
                  g.tgroup_name=%s g.tgroup_share=%d g.thread_count=%d \
                  epoch_count=%d tgroup_ideal=%f"
                 delay_s
                 (Thread.self () |> Thread.id |> string_of_int)
                 (Clock.Timer.span_to_s thread_ctx.time_running)
                 tgroup.tgroup_name tgroup.tgroup_share
                 (tgroup.thread_count |> Atomic.get)
                 (epoch_count |> Atomic.get)
                 (Clock.Timer.span_to_s tgroup.time_ideal) ;
               (*todo: do not sleep if this is the last thread in the tgroup(s) *)
               if tgroup.tgroup = Tgroup.Group.authenticated_root then
                 with_time_counter_now thread_last_yield Thread.yield
               else
                 with_time_counter_now thread_last_yield (fun () ->
                     let _ = Xapi_stdext_unix.Unixext.select [] [] [] delay_s in
                     ()
                 )
             )
           in

           (* fair scheduling decision to check if thread time_running has exceeded
              tgroup-mandated ideal time per thread *)
           if
             Clock.Timer.span_is_longer thread_ctx.time_running
               ~than:tgroup.time_ideal
           then
             let since_last_global_slice =
               Mtime_clock.count (last_yield |> Atomic.get)
             in
             let until_next_global_slice =
               Mtime.Span.abs_diff global_slice_period since_last_global_slice
             in
             let thread_delay_s =
               (* the delay is upperbounded to be no longer than the yield_interval *)
               if until_next_global_slice > global_slice_period then
                 Clock.Timer.span_to_s global_slice_period
               else
                 Clock.Timer.span_to_s until_next_global_slice
             in

             is_to_sleep_or_yield thread_delay_s
       )

  let incr_epoch ~frequency =
    let epoch = epoch_count |> Atomic.get in
    if epoch mod frequency = 0 then
      Atomic.set epoch_count 1
    else
      Atomic.incr epoch_count

  let sched_global_slice ~global_slice_period =
    incr_epoch ~frequency:1024 ;

    (*roughly every 10s for timeslices of 10ms*)

    (* goal is to recalculate thread.time_ideal for each thread: *)
    (* 1) fairness: each thread group get the same amount of time inside the slice  *)
    (* 2) control : each thread group time is then weighted by its tgroup_share     *)
    (* 3) delegate: later, asynchronously, each thread decides to maybe yield based on its thread group idea of ideal time per thread *)
    (*    delegation via tgroups minimizes the number of synchronous global writes here from O(threads) to O(groups) *)
    let time_ideal_of_tgroups groups =
      Tgroup.ThreadGroup.(
        let group_share_total =
          (*
            (* this static value would reserve cpu time to tgroups with no threads to run at the moment *)
            match (tgroup_total_share |> Atomic.get |> float_of_int) with 0. -> 1. | x -> x
            *)
          (* reserve cpu time only to tgroups that have threads to run at the moment *)
          groups |> List.fold_left (fun xs x -> x.tgroup_share + xs) 0
        in
        (*with_epoch_frequency_of ~every:1000 |> fun _ -> D.debug in*)
        groups
        |> List.iter (fun g ->
               let group_share_ratio =
                 match group_share_total with
                 | 0 ->
                     0.
                 | gst ->
                     (g.tgroup_share |> float_of_int) /. (gst |> float_of_int)
               in
               let group_time_ns =
                 group_share_ratio
                 *. (global_slice_period |> Mtime.Span.to_float_ns)
               in
               let thread_time_ideal =
                 match g.thread_count |> Atomic.get with
                 | 0 ->
                     0.
                 | gnt ->
                     group_time_ns /. (gnt |> float_of_int)
               in
               g.time_ideal <-
                 Mtime.Span.of_float_ns @@ thread_time_ideal |> Option.get ;
               D.debug
                 "runtime sched_global_slice: g.tgroup_name=%s \
                  g.tgroup_share=%d g.thread_count=%d g.time_ideal=%f ns \
                  epoch_count=%d group_share_ration=%f group_time_ns=%f \
                  tgroup_total_share=%d"
                 g.tgroup_name g.tgroup_share
                 (g.thread_count |> Atomic.get)
                 thread_time_ideal
                 (epoch_count |> Atomic.get)
                 group_share_ratio group_time_ns
                 (Tgroup.ThreadGroup.tgroup_total_share |> Atomic.get)
           )
      )
    in
    let tgroups_with_threads =
      List.fold_left
        (fun xs x ->
          if x.Tgroup.ThreadGroup.thread_count |> Atomic.get > 0 then
            x :: xs
          else
            xs
        )
        []
        (Tgroup.ThreadGroup.tgroups ())
    in
    tgroups_with_threads |> time_ideal_of_tgroups
end

let periodic_hook (_ : Gc.Memprof.allocation) =
  let () =
    try
      let yield_interval = Atomic.get yield_interval in
      if not (am_i_holding_locks ()) then
        let elapsed = Mtime_clock.count (Atomic.get last_yield) in
        if Clock.Timer.span_is_longer elapsed ~than:yield_interval then
          with_time_counter_now last_yield (fun () ->
              let () =
                Runtime.sched_global_slice ~global_slice_period:yield_interval
              in
              Thread.yield ()
          )
        else
          Runtime.maybe_thread_yield ~global_slice_period:yield_interval
    with _ ->
      (* It is not safe to raise exceptions here, it'd require changing all code to be safe to asynchronous interrupts/exceptions,
         see https://guillaume.munch.name/software/ocaml/memprof-limits/index.html#isolation
         Because this is just a performance optimization, we fall back to safe behaviour: do nothing, and just keep track that we failed
      *)
      Atomic.incr failures
  in
  None

let periodic =
  Gc.Memprof.
    {null_tracker with alloc_minor= periodic_hook; alloc_major= periodic_hook}

let set ?(sampling_rate = 1e-4) interval =
  Atomic.set yield_interval
    (Mtime.Span.of_float_ns @@ (interval *. 1e9) |> Option.get) ;
  Gc.Memprof.start ~sampling_rate ~callstack_size:0 periodic

let clear () =
  Gc.Memprof.stop () ;
  Atomic.set yield_interval Mtime.Span.zero
