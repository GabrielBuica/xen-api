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
  let ( // ) = Filename.concat in
  let dir_lst =
    [
      requests // string_of_endpoint Internal // "host" // "SM" // "x"
    ; requests // string_of_endpoint External
    ]
  in
  List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755) dir_lst

module Creator = struct
  type t = SM of endpoint * string | Normal of endpoint * string

  let default_creator = Normal (External, "default")

  let name = function SM _ -> "SM" | Normal _ -> "normal"

  let of_http_hdr =
    Option.map (function
      | "SM" ->
          SM (Internal, "host")
      | hdr ->
          Normal (External, hdr)
      )

  let to_cgroup = function
    | SM (endpoint, user) ->
        requests // string_of_endpoint endpoint // user // "SM" // "x"
    | Normal _ ->
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
end
