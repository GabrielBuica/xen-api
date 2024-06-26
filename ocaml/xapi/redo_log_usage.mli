(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(**
 * @group Redo-log
*)

val read_from_redo_log :
     [< `RO | `RW] Xapi_database.Redo_log.redo_log
  -> string
  -> Xapi_database.Db_ref.t
  -> unit
(** Connect to the block device and write the latest version of the database
 * on it to a file with a given name. *)

val stop_using_redo_log : _ Xapi_database.Redo_log.redo_log -> unit
(** Disconnect from the block device. May be safely called even when not currently connected. *)
