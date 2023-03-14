(*
 * Copyright (C) 2023 Cloud Software Group
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

module D = Debug.Make (struct let name = "tracing" end)

open D

type endpoint = Bugtool | Url of Uri.t

let url_file = "/etc/xapi-tracing-url"

let ok_none = Ok None

module SpanContext = struct
  type t = {trace_id: string; span_id: string} [@@deriving rpcty]
end

module Span = struct
  type t = {
      context: SpanContext.t
    ; parent: t option
    ; name: string
    ; begin_time: float
    ; end_time: float option
    ; tags: (string * string) list
  }
  [@@deriving rpcty]

  let generate_id n = String.init n (fun _ -> "0123456789abcdef".[Random.int 16])

  let start ?(tags = []) ~name ~parent () =
    let trace_id =
      match parent with
      | None ->
          generate_id 32
      | Some span_parent ->
          span_parent.context.trace_id
    in
    let span_id = generate_id 16 in
    let context : SpanContext.t = {trace_id; span_id} in
    (* Using gettimeofday over Mtime as it is better for sharing timestamps between the systems *)
    let begin_time = Unix.gettimeofday () in
    let end_time = None in
    {context; parent; name; begin_time; end_time; tags}

  let finish ?(tags = []) ~span () =
    {span with end_time= Some (Unix.gettimeofday ()); tags= span.tags @ tags}

  let to_string s = Rpcmarshal.marshal t.Rpc.Types.ty s |> Jsonrpc.to_string

  let of_string s =
    Jsonrpc.of_string s
    |> Rpcmarshal.unmarshal t.Rpc.Types.ty
    |> Result.to_option
end

module Spans = struct
  let lock = Mutex.create ()

  let spans = Hashtbl.create 100

  let max_spans = ref 1000

  let set_max_spans x = max_spans := x

  let max_traces = ref 1000

  let set_max_traces x = max_traces := x

  let finished_spans = Hashtbl.create 100

  let add_to_spans ~(span : Span.t) =
    let key = span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt spans key with
        | None ->
            if Hashtbl.length spans < !max_traces then
              Hashtbl.add spans key [span]
        | Some span_list ->
            if List.length span_list < !max_spans then
              Hashtbl.replace spans key (span :: span_list)
    )

  let remove_from_spans span =
    let key = span.Span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt spans key with
        | None ->
            debug "Span does not exist or already finished" ;
            None
        | Some span_list ->
            ( match
                List.filter (fun x -> x.Span.context <> span.context) span_list
              with
            | [] ->
                Hashtbl.remove spans key
            | filtered_list ->
                Hashtbl.replace spans key filtered_list
            ) ;
            Some span
    )

  let add_to_finished span =
    let key = span.Span.context.trace_id in
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        match Hashtbl.find_opt finished_spans key with
        | None ->
            if Hashtbl.length finished_spans < !max_traces then
              Hashtbl.add finished_spans key [span]
        | Some span_list ->
            if List.length span_list < !max_spans then
              Hashtbl.replace finished_spans key (span :: span_list)
    )

  let mark_finished span = Option.iter add_to_finished (remove_from_spans span)

  let span_is_finished x =
    match x with
    | None ->
        false
    | Some (span : Span.t) -> (
      match Hashtbl.find_opt finished_spans span.context.trace_id with
      | None ->
          false
      | Some span_list ->
          List.exists (fun x -> x = span) span_list
    )

  (** since copies the existing finished spans and then clears the existing spans as to only export them once  *)
  let since () =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        let copy = Hashtbl.copy finished_spans in
        Hashtbl.clear finished_spans ;
        copy
    )

  module GC = struct
    let lock = Mutex.create ()

    let span_timeout = ref 86400.

    let span_timeout_thread = ref None

    let gc_inactive_spans () =
      Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
          Hashtbl.filter_map_inplace
            (fun _ spanlist ->
              let filtered =
                List.filter_map
                  (fun span ->
                    let elapsed =
                      Unix.gettimeofday () -. span.Span.begin_time
                    in
                    if elapsed > !span_timeout *. 1000000. then (
                      debug "Tracing: Span %s timed out, forcibly finishing now"
                        span.Span.context.span_id ;
                      let span =
                        Span.finish ~span
                          ~tags:
                            [
                              ( "gc_inactive_span_timeout"
                              , string_of_float elapsed
                              )
                            ]
                          ()
                      in
                      add_to_finished span ; None
                    ) else
                      Some span
                  )
                  spanlist
              in
              match filtered with [] -> None | spans -> Some spans
            )
            spans
      )

    let initialise_thread ~timeout =
      span_timeout := timeout ;
      span_timeout_thread :=
        Some
          (Thread.create
             (fun () ->
               while true do
                 debug "Tracing: Span garbage collector" ;
                 Thread.delay !span_timeout ;
                 gc_inactive_spans ()
               done
             )
             ()
          )
  end
end

module TracerProvider = struct
  type t = {
      name_label: string
    ; tags: (string * string) list
    ; endpoints: endpoint list
    ; filters: string list
    ; processors: string list
    ; enabled: bool
    ; service_name: string
  }
end

module Tracer = struct
  type t = {name: string; provider: TracerProvider.t}

  let create ~name ~provider = {name; provider}

  let no_op =
    let provider : TracerProvider.t =
      {
        name_label= ""
      ; tags= []
      ; endpoints= []
      ; filters= []
      ; processors= []
      ; enabled= false
      ; service_name= ""
      }
    in
    {name= ""; provider}

  let start ~tracer:t ~name ~parent : (Span.t option, exn) result =
    (* Do not start span if the TracerProvider is diabled*)
    if not t.provider.enabled then
      ok_none
    else
      let tags = t.provider.tags in
      let span = Span.start ~tags ~name ~parent () in
      Spans.add_to_spans ~span ; Ok (Some span)

  let finish span =
    Ok
      (Option.map
         (fun span ->
           let span = Span.finish ~span () in
           Spans.mark_finished span ; span
         )
         span
      )

  let assert_finished x = Spans.assert_finished x
end

let lock = Mutex.create ()

let tracer_providers = Hashtbl.create 100

let get_tracer ~name =
  let providers =
    Xapi_stdext_threads.Threadext.Mutex.execute lock (fun () ->
        Hashtbl.fold (fun _k v acc -> v :: acc) tracer_providers []
    )
  in
  match
    List.find_opt (fun provider -> provider.TracerProvider.enabled) providers
  with
  | Some provider ->
      Tracer.create ~name ~provider
  | None ->
      warn "No provider found" ; Tracer.no_op

let enable_span_garbage_collector ?(timeout = 86400.) () =
  Spans.GC.initialise_thread ~timeout

module Export = struct
  module Content = struct
    module Json = struct
      module Zipkinv2 = struct
        module ZipkinSpan = struct
          type localEndpoint = {serviceName: string} [@@deriving rpcty]

          type t = {
              id: string
            ; traceId: string
            ; parentId: string option
            ; name: string
            ; timestamp: int
            ; duration: int
            ; kind: string
            ; localEndpoint: localEndpoint
            ; tags: (string * string) list
          }
          [@@deriving rpcty]

          type t_list = t list [@@deriving rpcty]

          let json_of_t_list s =
            Rpcmarshal.marshal t_list.Rpc.Types.ty s |> Jsonrpc.to_string
        end

        let zipkin_span_of_span : Span.t -> ZipkinSpan.t =
         fun s ->
          {
            id= s.context.span_id
          ; traceId= s.context.trace_id
          ; parentId= Option.map (fun x -> x.Span.context.span_id) s.parent
          ; name= s.name
          ; timestamp= int_of_float (s.begin_time *. 1000000.)
          ; duration=
              Option.value s.end_time ~default:(Unix.gettimeofday () *. 1000000.)
              -. s.begin_time
              |> ( *. ) 1000000.
              |> int_of_float
          ; kind= "SERVER"
          ; localEndpoint= {serviceName= "xapi"}
          ; tags= s.tags
          }

        let content_of (spans : Span.t list) =
          List.map zipkin_span_of_span spans |> ZipkinSpan.json_of_t_list
      end
    end
  end

  module Destination = struct
    module File = struct
      let trace_log_dir = ref "/var/log/dt/zipkinv2/json"

      let host_id = ref "localhost"

      let set_trace_log_dir dir = trace_log_dir := dir

      let set_host_id id = host_id := id

      let export ~trace_id ~span_json ~path : (string, exn) result =
        try
          let date = Ptime_clock.now () |> Ptime.to_rfc3339 ~frac_s:6 in
          let file =
            path
            ^ String.concat "-" [trace_id; "xapi"; !host_id; date]
            ^ ".json"
          in
          Xapi_stdext_unix.Unixext.mkdir_rec (Filename.dirname file) 0o700 ;
          Xapi_stdext_unix.Unixext.write_string_to_file file span_json ;
          Ok ""
        with e -> Error e
    end

    module Http = struct
      module Request = Cohttp.Request.Make (Cohttp_posix_io.Buffered_IO)
      module Response = Cohttp.Response.Make (Cohttp_posix_io.Buffered_IO)

      let export ~span_json ~url : (string, exn) result =
        try
          let body = span_json in
          let uri = Uri.of_string url in
          let headers =
            Cohttp.Header.of_list
              [
                ("accepts", "application/json")
              ; ("content-type", "application/json")
              ; ("content-length", string_of_int (String.length body))
              ]
          in
          Open_uri.with_open_uri uri (fun fd ->
              let request =
                Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri
              in
              let ic = Unix.in_channel_of_descr fd in
              let oc = Unix.out_channel_of_descr fd in
              Request.write
                (fun writer -> Request.write_body writer body)
                request oc ;
              Unix.shutdown fd Unix.SHUTDOWN_SEND ;
              match try Response.read ic with _ -> `Eof with
              | `Eof ->
                  Ok ""
              | `Invalid x ->
                  Error (Failure ("invalid read: " ^ x))
              | `Ok response ->
                  let body = Buffer.create 128 in
                  let reader = Response.make_body_reader response ic in
                  let rec loop () =
                    match Response.read_body_chunk reader with
                    | Cohttp.Transfer.Chunk x ->
                        Buffer.add_string body x ; loop ()
                    | Cohttp.Transfer.Final_chunk x ->
                        Buffer.add_string body x
                    | Cohttp.Transfer.Done ->
                        ()
                  in
                  loop () ;
                  Ok (Buffer.contents body)
          )
        with e -> Error e
    end

    let export_to_http_server () =
      debug "Tracing: About to export to http server" ;
      let url =
        Xapi_stdext_unix.Unixext.string_of_file url_file |> String.trim
      in
      let span_list = Spans.since () in
      try
        Hashtbl.iter
          (fun _ span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match Http.export ~span_json:zipkin_spans ~url with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list ;
        Ok ()
      with e -> Error e

    let export_to_logs () =
      debug "Tracing: About to export to dom0 logs" ;
      let span_list = Spans.since () in
      try
        Hashtbl.iter
          (fun trace_id span_list ->
            let zipkin_spans = Content.Json.Zipkinv2.content_of span_list in
            match
              File.export ~trace_id ~span_json:zipkin_spans
                ~path:!File.trace_log_dir
            with
            | Ok _ ->
                ()
            | Error e ->
                raise e
          )
          span_list ;
        Ok ()
      with e -> Error e

    let main () =
      Thread.create
        (fun () ->
          while true do
            debug "Tracing: Waiting 30s before exporting spans" ;
            Thread.delay 30. ;
            match export_to_http_server () with
            | Ok () ->
                ()
            | Error e ->
                debug "Tracing: Export ERROR %s" (Printexc.to_string e)
          done
        )
        ()
  end
end

let main = Export.Destination.main
