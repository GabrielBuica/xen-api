(*
 * Copyright (C) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT Group WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let ( // ) = Filename.concat

let requests = "/sys/fs/cgroup/cpu/control.slice/xapi.service/request"

module Pthread = struct
  type t = string option

  let self () : t =
    let unix_pid_tid = Unix.readlink "/proc/thread-self" in
    match String.split_on_char '/' unix_pid_tid with
    | [_; _; tid] ->
        Some tid
    | _ ->
        (* debug "thread-self: can't parse: %s" unix_pid_tid ; *)
        None

  external c_set_name : string -> int = "stub_pthread_set_name"

  let set_name s =
    let len = String.length s in
    let len = if len > 15 then 15 else len in
    let tname = String.sub s 0 len in
    match c_set_name tname with 0 -> Some tname | _ -> None

  external get_name : unit -> string option = "stub_pthread_get_name"

  let get_cgroup () =
    let self = match self () with Some s -> s | None -> "self" in
    let lines =
      Xapi_stdext_unix.Unixext.read_lines
        ~path:(Printf.sprintf "/proc/%s/cgroup" self)
    in
    lines
    |> List.find_map (fun elt ->
           match String.split_on_char ':' elt with
           | [_; "cpu,cpuacct"; group] ->
               Some group
           | _ ->
               None
       )
    |> Option.value ~default:(String.concat "\n" lines)
end

module Group = struct
  module SM = struct
    type t

    let name = "SM"

    let cpu_shares = "65536"
  end

  module Internal = struct
    type t

    let name = "internal"
  end

  module External = struct
    type t

    let name = "external"

    let cpu_shares = "128"
  end

  module Host = struct
    type t

    let name = "host"
  end

  module Server = struct
    type t

    let name = "server"

    let cpu_shares = "65536"
  end

  module Cli = struct
    type t

    let name = "cli"

    let cpu_shares = "128"
  end

  type _ group =
    | Internal_Host_SM : (Internal.t * Host.t * SM.t) group
    | EXTERNAL : External.t group
    | Internal_Server : (Internal.t * Server.t) group
    | Internal_Cli : (Internal.t * Cli.t) group

  type t = Group : 'a group -> t

  let all =
    [
      Group Internal_Host_SM
    ; Group EXTERNAL
    ; Group Internal_Server
    ; Group Internal_Cli
    ]

  let to_cgroup : type a. a group -> string = function
    | Internal_Host_SM ->
        Internal.name // Host.name // SM.name
    | EXTERNAL ->
        External.name
    | Internal_Server ->
        Internal.name // Server.name
    | Internal_Cli ->
        Internal.name // Cli.name

  module Originator = struct
    type t = Internal_Host_SM | EXTERNAL | Internal_Server | Internal_Cli

    let of_string = function
      | s
        when String.equal
               (String.lowercase_ascii SM.name)
               (String.lowercase_ascii s) ->
          Internal_Host_SM
      | s
        when String.equal
               (String.lowercase_ascii Cli.name)
               (String.lowercase_ascii s) ->
          Internal_Cli
      | s
        when String.equal
               (String.lowercase_ascii External.name)
               (String.lowercase_ascii s) ->
          EXTERNAL
      | _ ->
          EXTERNAL

    let to_string = function
      | Internal_Host_SM ->
          SM.name
      | Internal_Server ->
          Server.name
      | Internal_Cli ->
          Cli.name
      | EXTERNAL ->
          External.name
  end

  let of_originator = function
    | Originator.Internal_Host_SM ->
        Group Internal_Host_SM
    | Originator.Internal_Server ->
        Group Internal_Server
    | Originator.Internal_Cli ->
        Group Internal_Cli
    | Originator.EXTERNAL ->
        Group EXTERNAL

  let get_originator = function
    | Group Internal_Host_SM ->
        Originator.Internal_Host_SM
    | Group EXTERNAL ->
        Originator.EXTERNAL
    | Group Internal_Server ->
        Originator.Internal_Server
    | Group Internal_Cli ->
        Originator.Internal_Cli
end

module Cgroup = struct
  open Group

  let dir_of = function Group group -> requests // to_cgroup group

  let write_to_file filename tid =
    let fd =
      Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640
    in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () ->
        let buf = "0\n" in
        let len = String.length buf in
        if Unix.write fd (Bytes.unsafe_of_string buf) 0 len <> len then ()
        (* debug "tid_write %s > %s failed" tid filename *)
      )
      (fun () -> Unix.close fd)

  let attach_task group tid =
    let tasks_file = dir_of group // "tasks" in
    write_to_file tasks_file tid

  let set_cpu_shares group share =
    let tasks_file = dir_of group // "cpu.shares" in
    write_to_file tasks_file share

  let init () =
    all
    |> List.map (function Group elt -> requests // to_cgroup elt)
    |> List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755) ;
    set_cpu_shares (Group EXTERNAL) Group.External.cpu_shares ;
    set_cpu_shares (Group Internal_Host_SM) Group.SM.cpu_shares ;
    set_cpu_shares (Group Internal_Server) Group.Server.cpu_shares

  let set_cur_cgroup ~originator =
    match Pthread.self () with
    | None ->
        ()
    | Some tid -> (
      match originator with
      | Originator.Internal_Host_SM ->
          attach_task (Group Internal_Host_SM) tid
      | Originator.Internal_Server ->
          attach_task (Group Internal_Server) tid
      | Originator.Internal_Cli ->
          attach_task (Group Internal_Cli) tid
      | Originator.EXTERNAL ->
          attach_task (Group EXTERNAL) tid
    )

  let of_originator originator = set_cur_cgroup ~originator
end

module Priority = struct
  (* type policy_t = SCHED_OTHER | SCHED_FIFO | SCHED_RR *)
  type policy_t = SCHED_RR

  let chrt ~(_policy : policy_t) ~(_tid : Pthread.t) (_priority : int) = ()
  (* call chrt -r -p priority tid *)
  (* in the future use a syscall *)

  let of_originator = function
    | Group.Originator.Internal_Host_SM ->
        chrt ~_policy:SCHED_RR ~_tid:(Pthread.self ()) 70
    | Group.Originator.Internal_Server ->
        chrt ~_policy:SCHED_RR ~_tid:(Pthread.self ()) 80
    | Group.Originator.EXTERNAL ->
        ()
    | Group.Originator.Internal_Cli ->
        ()
end

type state = {
    _originator: Group.Originator.t option
  ; _tid: Pthread.t
  ; _cgroup_dir: string option
}

let empty_state = {_originator= None; _tid= None; _cgroup_dir= None}

let of_originator originator =
  Cgroup.of_originator originator ;
  Priority.of_originator originator

let of_req_originator originator =
  originator
  |> Option.value ~default:Group.Originator.(to_string EXTERNAL)
  |> Group.Originator.of_string
  |> of_originator
