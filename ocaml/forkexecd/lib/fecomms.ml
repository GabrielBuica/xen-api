module Unixext = Xapi_stdext_unix.Unixext

module EnvHelpers = struct
  let traceparent_key = "TRACEPARENT"

  let of_traceparent traceparent =
    match traceparent with
    | None ->
        []
    | Some traceparent ->
        [String.concat "=" [traceparent_key; traceparent]]

  let to_traceparent env =
    let env_opt =
      List.find_opt (String.starts_with ~prefix:traceparent_key) env
    in
    Option.bind env_opt (fun key_value ->
        match String.split_on_char '=' key_value with
        | ["TRACEPARENT"; traceparent] ->
            Some traceparent
        | _ ->
            None
    )

  let of_span span =
    match span with
    | None ->
        []
    | Some span ->
        Some
          (span
          |> Tracing.Span.get_context
          |> Tracing.SpanContext.to_traceparent
          )
        |> of_traceparent
end

let update_ferpc_env tracing (ferpc : Fe.ferpc) : Fe.ferpc =
  match ferpc with
  | Setup setup_cmd ->
      let env = setup_cmd.env @ EnvHelpers.of_span tracing in
      Setup {setup_cmd with env}
  | Setup_response _ ->
      ferpc
  | Exec ->
      ferpc
  | Execed _ ->
      ferpc
  | Finished _ ->
      ferpc
  | Dontwaitpid ->
      ferpc

let with_tracing ~tracing ~name f =
  let name = Printf.sprintf "fecomms.%s" name in
  Tracing.with_tracing ~parent:tracing ~name f

let open_unix_domain_sock ?tracing () =
  with_tracing ~tracing ~name:"open_unix_domain_sock" @@ fun _ ->
  Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0

let open_unix_domain_sock_server ?tracing path =
  with_tracing ~tracing ~name:"open_unix_domain_sock_server" @@ fun tracing ->
  Unixext.mkdir_rec (Filename.dirname path) 0o755 ;
  Unixext.unlink_safe path ;
  let sock = open_unix_domain_sock ?tracing () in
  try
    Unix.bind sock (Unix.ADDR_UNIX path) ;
    Unix.listen sock 5 ;
    sock
  with e -> Unix.close sock ; raise e

let open_unix_domain_sock_client ?tracing path =
  with_tracing ~tracing ~name:"open_unix_domain_sock_client" @@ fun tracing ->
  let sock = open_unix_domain_sock ?tracing () in
  try
    Unix.connect sock (Unix.ADDR_UNIX path) ;
    sock
  with e -> Unix.close sock ; raise e

let read_raw_rpc ?tracing sock =
  with_tracing ~tracing ~name:"read_raw_rpc" @@ fun _ ->
  let buffer = Bytes.make 12 '\000' in
  ( with_tracing ~tracing ~name:"Unixext.really_read_string1" @@ fun _ ->
    Unixext.really_read sock buffer 0 12
  ) ;
  let header = Bytes.unsafe_to_string buffer in
  match int_of_string_opt header with
  | Some len ->
      let body =
        with_tracing ~tracing ~name:"Unixext.really_read_string2" @@ fun _ ->
        Unixext.really_read_string sock len
      in
      Ok (Fe.ferpc_of_rpc (Jsonrpc.of_string body))
  | None ->
      Unix.(shutdown sock SHUTDOWN_ALL) ;
      Error ("Header is not an integer: " ^ header)

let write_raw_rpc ?tracing sock ferpc =
  with_tracing ~tracing ~name:"write_raw_rpc" @@ fun tracing ->
  let ferpc = update_ferpc_env tracing ferpc in
  let body = Jsonrpc.to_string (Fe.rpc_of_ferpc ferpc) in
  let len = String.length body in
  let buffer = Printf.sprintf "%012d%s" len body in
  Unixext.really_write_string sock buffer

exception Connection_closed

let receive_named_fd ?tracing sock =
  with_tracing ~tracing ~name:"receive_named_fd" @@ fun _ ->
  let buffer = Bytes.make 36 '\000' in
  let len, _from, newfd = Unixext.recv_fd sock buffer 0 36 [] in
  let buffer = Bytes.unsafe_to_string buffer in
  if len = 0 then raise Connection_closed ;
  (newfd, buffer)

let send_named_fd ?tracing sock uuid fd =
  with_tracing ~tracing ~name:"send_named_fd" @@ fun _ ->
  ignore (Unixext.send_fd_substring sock uuid 0 (String.length uuid) [] fd)
