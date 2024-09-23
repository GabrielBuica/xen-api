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

module D = Debug.Make (struct let name = __MODULE__ end)

open D

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
        debug "thread-self: can't parse: %s" unix_pid_tid ;
        None
end

module Group = struct
  module SM = struct
    type t

    let name = "sm"
  end

  module Internal = struct
    type t

    let name = "internal"
  end

  module External = struct
    type t

    let name = "external"
  end

  module Host = struct
    type t

    let name = "host"
  end

  type _ group =
    | Internal_Host_SM : (Internal.t * Host.t * SM.t) group
    | EXTERNAL : External.t group

  type t = Group : 'a group -> t

  let to_cgroup : type a. a group -> string = function
    | Internal_Host_SM ->
        Internal.name // Host.name // SM.name
    | EXTERNAL ->
        External.name

  let all = [Group Internal_Host_SM; Group EXTERNAL]

  let init () =
    all
    |> List.map (function Group elt -> requests // to_cgroup elt)
    |> List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755)

  let dir_of = function Group group -> requests // to_cgroup group

  let of_originator : string -> t = function
    | "SM" ->
        Group Internal_Host_SM
    | _ ->
        Group EXTERNAL

  let write_tid_to_tasks filename tid =
    let fd =
      Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640
    in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () ->
        let buf = tid ^ "\n" in
        let len = String.length buf in
        if Unix.write fd (Bytes.unsafe_of_string buf) 0 len <> len then
          debug "tid_write %s > %s failed" tid filename
      )
      (fun () -> Unix.close fd)

  let attach_task group tid =
    let tasks_file = (group |> dir_of) // "tasks" in
    write_tid_to_tasks tasks_file tid
end

type state = {
    _originator: string option
  ; _tid: string option
  ; _cgroup_dir: string option
}

let empty_state = {_originator= None; _tid= None; _cgroup_dir= None}

let set_cur_cgroup ~originator =
  match (originator, Pthread.self ()) with
  | "SM", Some tid ->
      Group.attach_task (Group Internal_Host_SM) tid
  | _, Some tid ->
      Group.attach_task (Group EXTERNAL) tid
  | _ ->
      ()
