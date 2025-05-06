let with_updated_tgroup ?identity ?intrapool f =
  let tgroup =
    Tgroup.of_creator (Tgroup.Description.Creator.make ?identity ?intrapool ())
  in
  let open Xapi_stdext_threads.Threadext in
  let thread_ctx = ThreadRuntimeContext.get () in
  (* authenticated_root here should mean a group has not been set yet and
     we should set one. otherwise go with what has already been set.*)
  if
    thread_ctx.tgroup = Tgroup.Description.authenticated_root
    || thread_ctx.tgroup = Tgroup.Description.unauthenticated
  then
    ThreadRuntimeContext.update
      (fun thread_ctx -> {thread_ctx with tgroup})
      thread_ctx ;
  Tgroup.with_one_thread_of_group tgroup f
