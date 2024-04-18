open Fe_debug

let setup ?tracing sock cmdargs id_to_fd_map syslog_stdout
    redirect_stderr_to_stdout env =
  let tracing =
    match
      Tracing.Tracer.start
        ~tracer:(Tracing.get_tracer ~name:"fe_main.setup")
        ~attributes:[] ~name:"setup" ~parent:tracing ()
    with
    | Ok span ->
        span
    | _ ->
        None
  in
  let fd_sock_path =
    Printf.sprintf "%s/fd_%s" Forkhelpers.temp_dir_server
      Uuidx.(to_string (make ()))
  in
  let fd_sock = Fecomms.open_unix_domain_sock ?tracing () in
  Xapi_stdext_unix.Unixext.unlink_safe fd_sock_path ;
  debug "About to bind to %s" fd_sock_path ;
  Unix.bind fd_sock (Unix.ADDR_UNIX fd_sock_path) ;
  Unix.listen fd_sock 5 ;
  debug "bound, listening" ;
  let result = Unix.fork () in
  if result = 0 then (
    debug "Child here!" ;
    let result2 = Unix.fork () in
    if result2 = 0 then (
      debug "Grandchild here!" ;
      (* Grandchild *)
      let state =
        {
          Child.cmdargs
        ; env
        ; id_to_fd_map
        ; syslog_stdout=
            {
              Child.enabled= syslog_stdout.Fe.enabled
            ; Child.key= syslog_stdout.Fe.key
            }
        ; redirect_stderr_to_stdout
        ; ids_received= []
        ; fd_sock2= None
        ; finished= false
        }
      in
      let response = Child.run state sock fd_sock fd_sock_path in
      ignore @@ Tracing.Tracer.finish tracing ;
      response
    ) else (* Child *)
      let _ = Tracing.Tracer.finish tracing in
      exit 0
  ) else (
    (* Parent *)
    debug "Waiting for process %d to exit" result ;
    ignore (Unix.waitpid [] result) ;
    Unix.close fd_sock ;
    let response = Some {Fe.fd_sock_path} in
    ignore @@ Tracing.Tracer.finish tracing ;
    response
  )

let systemd_managed () = try Daemon.booted () with Unix.Unix_error _ -> false

let _ =
  Tracing.create ~enabled:true ~attributes:[] ~endpoints:[Tracing.bugtool_name]
    ~name_label:"forkexecd"
    ~uuid:(Uuidx.to_string (Uuidx.make ())) ;
  Tracing_export.set_service_name "forkexecd" ;
  Tracing_export.main ()

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;

  let test_path =
    Option.bind (Sys.getenv_opt "FE_TEST") (fun _ ->
        Sys.getenv_opt "XDG_RUNTIME_DIR"
    )
  in

  let runtime_path = Option.value ~default:"/var" test_path in
  let main_sock =
    Fecomms.open_unix_domain_sock_server (runtime_path ^ "/xapi/forker/main")
  in
  Xapi_stdext_unix.Unixext.mkdir_rec Forkhelpers.temp_dir_server 0o755 ;

  if systemd_managed () then
    Daemon.notify Daemon.State.Ready |> ignore ;

  (* At this point the init.d script should return and we are listening on our socket. *)
  while true do
    try
      let sock, _addr = Unix.accept main_sock in
      reset () ;
      let cmd = Fecomms.read_raw_rpc sock in
      match cmd with
      | Ok (Fe.Setup s) -> (
          let tracing =
            Tracing.Tracer.span_of_traceparent
              ~traceparent:(Fecomms.EnvHelpers.to_traceparent s.Fe.env)
              ~name:"forkexecd.cmd"
          in
          let result =
            setup ?tracing sock s.Fe.cmdargs s.Fe.id_to_fd_map
              s.Fe.syslog_stdout s.Fe.redirect_stderr_to_stdout s.Fe.env
          in
          match result with
          | Some response ->
              ( try
                  Fecomms.write_raw_rpc ?tracing sock
                    (Fe.Setup_response response)
                with Unix.Unix_error (Unix.EPIPE, _, _) -> ()
              ) ;
              Unix.close sock
          | _ ->
              ()
        )
      | Ok msg ->
          debug "Ignoring invalid message (%s)" (Fe.ferpc_to_string msg) ;
          Unix.close sock
      | Error msg ->
          debug "Ignoring invalid message (%s)" msg ;
          Unix.close sock
    with e -> debug "Caught exception at top level: %s" (Printexc.to_string e)
  done
