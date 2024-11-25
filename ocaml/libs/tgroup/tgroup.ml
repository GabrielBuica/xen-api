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

module Group = struct
  module Internal = struct
    type t

    let name = "internal"
  end

  module External = struct
    type t

    let name = "external"

    module Intrapool = struct
      type t

      let name = "intrapool"
    end

    module Authenticated = struct
      type t = string

      let name = "authenticated"
    end

    module Unauthenticated = struct
      type t

      let name = "unauthenticated"
    end
  end

  module SM = struct
    type t

    let name = "SM"
  end

  module CLI = struct
    type t

    let name = "cli"
  end

  module Identity = struct
    type t = string

    let to_string s : string = s
  end

  type _ group =
    | Internal_SM : (Internal.t * SM.t) group
    | Internal_CLI : (Internal.t * CLI.t) group
    | External_Intrapool : (External.t * External.Intrapool.t) group
    | External_Authenticated :
        Identity.t
        -> (External.t * External.Authenticated.t) group
    | External_Unautheticated : (External.t * External.Unauthenticated.t) group

  type t = Group : 'a group -> t

  let all =
    [
      Group Internal_SM
    ; Group Internal_CLI
    ; Group External_Intrapool
    ; Group (External_Authenticated "root")
    ; Group External_Unautheticated
    ]

  module Kind = struct
    type t = Intrapool | Authenticated of Identity.t | Unautheticated

    let to_string = function
      | Intrapool ->
          External.Intrapool.name
      | Authenticated identity ->
          External.Authenticated.name // Identity.to_string identity
      | Unautheticated ->
          External.Unauthenticated.name
  end

  module Originator = struct
    type t = Internal_SM | Internal_CLI | External

    let of_string = function
      | s
        when String.equal
               (String.lowercase_ascii SM.name)
               (String.lowercase_ascii s) ->
          Internal_SM
      | s
        when String.equal
               (String.lowercase_ascii CLI.name)
               (String.lowercase_ascii s) ->
          Internal_CLI
      | _ ->
          External

    let to_string = function
      | Internal_SM ->
          SM.name
      | Internal_CLI ->
          CLI.name
      | External ->
          External.name
  end

  module Creator = struct
    type t = {endpoint: string; kind: Kind.t; originator: Originator.t}

    let make ?_user ?(intrapool = false) ?subject originator =
      let originator = Originator.of_string originator in
      if intrapool then
        {endpoint= Internal.name; kind= Kind.Intrapool; originator}
      else
        let endpoint =
          match originator with
          | Internal_CLI | Internal_SM ->
              Internal.name
          | External ->
              External.name
        in
        let kind =
          match subject with
          | None ->
              Kind.Unautheticated
          | Some sid ->
              Kind.Authenticated sid
        in
        {endpoint; kind; originator}

    let default_creator =
      {
        endpoint= External.name
      ; kind= Kind.Authenticated "root"
      ; originator= Originator.External
      }

    let to_string c =
      Printf.sprintf "Creator -> kind:%s endpoint:%s originator:%s"
        (Kind.to_string c.kind) c.endpoint
        (Originator.to_string c.originator)
  end

  let get_originator = function
    | Group Internal_SM ->
        Originator.Internal_SM
    | Group Internal_CLI ->
        Originator.Internal_CLI
    | _ ->
        Originator.External

  let of_creator creator =
    match creator.Creator.originator with
    | Internal_SM ->
        Group Internal_SM
    | Internal_CLI ->
        Group Internal_CLI
    | External -> (
      match creator.Creator.kind with
      | Authenticated identity ->
          Group (External_Authenticated identity)
      | Intrapool ->
          Group External_Intrapool
      | Unautheticated ->
          Group External_Unautheticated
    )

  let to_cgroup : type a. a group -> string = function
    | Internal_SM ->
        Internal.name // SM.name
    | Internal_CLI ->
        Internal.name // CLI.name
    | External_Authenticated identity ->
        External.name
        // External.Authenticated.name
        // Identity.to_string identity
    | External_Intrapool ->
        External.name // External.Intrapool.name
    | External_Unautheticated ->
        External.name // External.Unauthenticated.name
end

module Cgroup = struct
  type t = string

  let cgroup_dir = Atomic.make None

  let dir_of group : t option =
    match group with
    | Group.Group group ->
        Option.map
          (fun dir -> dir // Group.to_cgroup group)
          (Atomic.get cgroup_dir)

  let with_dir dir f arg =
    Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755 ;
    f arg

  let write_cur_tid_to_cgroup_file filename =
    try
      let perms = 0o640 in
      let mode = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
      Xapi_stdext_unix.Unixext.with_file filename mode perms @@ fun fd ->
      (* Writing 0 to the task file will automatically transform in writing
         the current caller tid to the file.

         Writing 0 to the processes file will automatically write the caller's
         pid to file. *)
      let buf = "0\n" in
      let len = String.length buf in
      if Unix.write fd (Bytes.unsafe_of_string buf) 0 len <> len then
        warn "writing current tid to %s failed" filename
    with exn ->
      warn "writing current tid to %s failed with exception: %s" filename
        (Printexc.to_string exn)

  let attach_task group =
    Option.iter
      (fun dir ->
        let tasks_file = dir // "tasks" in
        with_dir dir write_cur_tid_to_cgroup_file tasks_file
      )
      (dir_of group)

  let set_cur_cgroup ~creator = attach_task (Group.of_creator creator)

  let set_cgroup creator = set_cur_cgroup ~creator

  let init dir =
    let () = Atomic.set cgroup_dir (Some dir) in
    Group.all
    |> List.filter_map dir_of
    |> List.iter (fun dir -> with_dir dir debug "created cgroup for: %s" dir) ;
    set_cur_cgroup ~creator:Group.Creator.default_creator
end

let of_req_originator originator =
  Option.iter
    (fun _ ->
      try
        originator
        |> Option.value ~default:Group.External.name
        |> Group.Creator.make
        |> Cgroup.set_cgroup
      with _ -> ()
    )
    (Atomic.get Cgroup.cgroup_dir)

let of_creator creator = creator |> Cgroup.set_cgroup
