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

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let ( // ) = Filename.concat

type endpoint = Internal | External

let string_of_endpoint = function
  | Internal ->
      "internal"
  | External ->
      "external"

let requests = "/sys/fs/cgroup/cpu/control.slice/xapi.service/request"

let init () =
  let dir_lst =
    [
      requests // string_of_endpoint Internal // "host" // "SM" // "x"
    ; requests // string_of_endpoint External
    ]
  in
  List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755) dir_lst

(* module Creator = struct
     type t = Internal_Host_SM of string | External [@@deriving fold]

     let default_creator = External

     let name = function Internal_Host_SM _ -> "SM" | External _ -> "external"



     let to_cgroup = function
       | SM (endpoint, user) ->
           requests // string_of_endpoint endpoint // user // "SM" // "x"
       | External ->
           requests // string_of_endpoint External

     let set_priority creator =
       let pthread_self () = Unix.readlink "/proc/thread-self" in
       let pid = (Unix.getpid () |> string_of_int) ^ "/n" in
       let dir = to_cgroup creator in
       let task_file = dir // "tasks" in
       debug "task file: %s with pid: %s and pthread: %s" task_file pid
         (pthread_self ()) ;

       let ( let@ ) f x = f x in

       let with_fd file_name =
         Xapi_stdext_unix.Unixext.with_file file_name
           [O_WRONLY; O_CREAT; O_APPEND]
           0o700
       in

       let write fd str =
         let content = str ^ "\n" in
         ignore @@ Unix.write_substring fd content 0 (String.length content)
       in

       let@ fd = task_file |> with_fd in
       write fd pid
   end *)
module Pthread = struct
  type t = string option

  let self () : t =
    let unix_pid_tid = Unix.readlink "/proc/thread-self" in
    match String.split_on_char '/' unix_pid_tid with
    | [pid; _; tid] ->
        Some tid
    | _ ->
        debug "thread-self: can't parse: %s" unix_pid_tid ;
        None
end

module Group = struct
  module SM = struct
    type t

    let name = __MODULE__
  end

  module Internal = struct
    type t

    let name = __MODULE__
  end

  module External = struct
    type t

    let name = __MODULE__
  end

  module Host = struct
    type t

    let name = __MODULE__
  end

  type _ group =
    | Internal_Host_SM : (Internal.t * Host.t * SM.t) group
    | EXTERNAL : External.t group

  type any_group = Any : 'a group -> any_group

  let to_cgroup : type a. a group -> string = function
    | Internal_Host_SM ->
        Internal.name // Host.name // SM.name
    | EXTERNAL ->
        External.name

  let all = [Any Internal_Host_SM; Any EXTERNAL]

  let init () =
    all
    |> List.map (function Any elt -> requests // to_cgroup elt)
    |> List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755)

  let dir_of group = requests // to_cgroup group

  let of_originator : string -> any_group = function
    | "SM" ->
        Any Internal_Host_SM
    | originator ->
        Any EXTERNAL

  let write_tid_to_tasks filename tid =
    let fd =
      Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640
    in
    finally
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
    originator: string option
  ; tid: string option
  ; cgroup_dir: string option
}

let empty_state = {originator= None; tid= None; cgroup_dir= None}

let set_cur_cgroup ~originator =
  match (originator, Pthread.self ()) with
  | "SM", Some tid ->
      Group.attach_task Internal_Host_SM tid
  | _, Some tid ->
      Group.attach_task EXTERNAL tid
  | _ ->
      ()
