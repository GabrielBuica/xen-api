(*
* Copyright (C) 2024 Cloud Software Group
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
include Map.Make (String)

let of_list list = List.to_seq list |> of_seq

let to_assoc_list attr = to_seq attr |> List.of_seq
