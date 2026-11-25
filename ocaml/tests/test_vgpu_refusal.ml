(* Every code Vgpu_refusal.to_api_error produces must be declared in
   Api_errors, or a client receives an error string it cannot look up. That
   the code also has a datamodel entry is checked by
   ocaml/idl/test_error_registration.ml. *)

[@@@warning "@8"]

open Vgpu_refusal

(* Fails to compile when a constructor is added, as a reminder to add it to
   the golden table below. *)
let _every_constructor_is_accounted_for (r : t) =
  match r with
  | Type_not_supported _ ->
      ()
  | Type_not_enabled _ ->
      ()
  | Type_incompatible_with_running _ ->
      ()
  | Insufficient_capacity _ ->
      ()
  | Group_contains_no_pgpus _ ->
      ()
  | Unexpected _ ->
      ()

let pgpu = "OpaqueRef:11111111-1111-1111-1111-111111111111"

let vgpu_type = "OpaqueRef:22222222-2222-2222-2222-222222222222"

let other_type = "OpaqueRef:33333333-3333-3333-3333-333333333333"

let gpu_group = "OpaqueRef:44444444-4444-4444-4444-444444444444"

(* The code and parameters each reason maps to, written out by hand so that a
   change to either shows up as a diff here. *)
let golden =
  [
    ( "Type_not_supported"
    , Type_not_supported {vgpu_type; supported= [other_type]}
    , "VGPU_TYPE_NOT_SUPPORTED"
    , [vgpu_type; other_type]
    )
  ; ( "Type_not_supported with nothing supported"
    , Type_not_supported {vgpu_type; supported= []}
    , "VGPU_TYPE_NOT_SUPPORTED"
    , [vgpu_type]
    )
  ; ( "Type_not_enabled"
    , Type_not_enabled {vgpu_type; enabled= [other_type]}
    , "VGPU_TYPE_NOT_ENABLED"
    , [vgpu_type; other_type]
    )
  ; ( "Type_not_enabled with nothing enabled"
    , Type_not_enabled {vgpu_type; enabled= []}
    , "VGPU_TYPE_NOT_ENABLED"
    , [vgpu_type]
    )
  ; ( "Type_incompatible_with_running"
    , Type_incompatible_with_running {pgpu; vgpu_type; running= other_type}
    , "VGPU_TYPE_NOT_COMPATIBLE_WITH_RUNNING_TYPE"
    , [pgpu; vgpu_type; other_type]
    )
  ; ( "Insufficient_capacity"
    , Insufficient_capacity {pgpu; vgpu_type}
    , "PGPU_INSUFFICIENT_CAPACITY_FOR_VGPU"
    , [pgpu; vgpu_type]
    )
  ; ( "Group_contains_no_pgpus"
    , Group_contains_no_pgpus {gpu_group}
    , "GPU_GROUP_CONTAINS_NO_PGPUS"
    , [gpu_group]
    )
  ]

exception Probe

let unexpected_cases =
  [
    ("a database failure", Db_exn.DBCache_NotFound ("x", "y", "z"))
  ; ("a division by zero on a size-0 type", Division_by_zero)
  ; ("an exception from nowhere in particular", Probe)
  ; ( "an API error raised deeper down"
    , Api_errors.Server_error (Api_errors.internal_error, ["boom"])
    )
  ]

let declared = List.fold_left (fun acc c -> c :: acc) [] !Api_errors.errors

let is_declared code = List.mem code declared

let describe = Printf.sprintf

(* The order best_refusal must report in, written out by hand rather than
   read from rank: the most useful first. The match is exhaustive, so a new
   constructor does not compile until it is given a place here. *)
let expected_position = function
  | Type_incompatible_with_running _ ->
      0
  | Type_not_enabled _ ->
      1
  | Type_not_supported _ ->
      2
  | Insufficient_capacity _ ->
      3
  | Group_contains_no_pgpus _ ->
      4
  | Unexpected _ ->
      5

(* One reason of each kind, most useful first. *)
let one_of_each =
  [
    ( "Type_incompatible_with_running"
    , Type_incompatible_with_running {pgpu; vgpu_type; running= other_type}
    )
  ; ("Type_not_enabled", Type_not_enabled {vgpu_type; enabled= [other_type]})
  ; ( "Type_not_supported"
    , Type_not_supported {vgpu_type; supported= [other_type]}
    )
  ; ("Insufficient_capacity", Insufficient_capacity {pgpu; vgpu_type})
  ; ("Group_contains_no_pgpus", Group_contains_no_pgpus {gpu_group})
  ; ("Unexpected", Unexpected Probe)
  ]

let name_of_best = function
  | None ->
      "nothing"
  | Some r -> (
    match List.find_opt (fun (_, r') -> r' = r) one_of_each with
    | Some (name, _) ->
        name
    | None ->
        to_string r
  )

let picks expected rs =
  match best_refusal rs with Some r -> r = expected | None -> false

let best_refusal_checks =
  let pairs =
    List.concat_map
      (fun (i, (better_name, better)) ->
        List.filteri (fun j _ -> j > i) one_of_each
        |> List.concat_map (fun (worse_name, worse) ->
            [
              ( describe "[%s; %s] reports %s, not %s" better_name worse_name
                  better_name
                  (name_of_best (best_refusal [better; worse]))
              , picks better [better; worse]
              )
            ; ( describe "[%s; %s] reports %s, not %s" worse_name better_name
                  better_name
                  (name_of_best (best_refusal [worse; better]))
              , picks better [worse; better]
              )
            ]
        )
      )
      (List.mapi (fun i x -> (i, x)) one_of_each)
  in
  let full = Insufficient_capacity {pgpu; vgpu_type} in
  let conflict =
    Type_incompatible_with_running
      {pgpu= gpu_group; vgpu_type; running= other_type}
  in
  let conflict' =
    Type_incompatible_with_running {pgpu; vgpu_type; running= other_type}
  in
  List.concat
    [
      [
        ( "the hand-written order lists every constructor once, in order"
        , List.mapi (fun i (_, r) -> expected_position r = i) one_of_each
          |> List.for_all Fun.id
        )
      ; ( describe "rank orders the reasons as written out here"
        , List.for_all
            (fun (_, a) ->
              List.for_all
                (fun (_, b) ->
                  compare (rank a) (rank b)
                  = compare (expected_position b) (expected_position a)
                )
                one_of_each
            )
            one_of_each
        )
      ; ("an empty list has no best refusal", best_refusal [] = None)
      ; ( describe
            "one PGPU full and another running a different type reports the \
             type conflict, not %s"
            (name_of_best (best_refusal [full; conflict]))
        , picks conflict [full; conflict]
        )
      ; ( "a tie goes to the first in the list"
        , picks conflict [conflict; conflict']
          && picks conflict' [conflict'; conflict]
        )
      ; ( "a tie behind a better reason still loses to it"
        , picks conflict [full; full; conflict]
        )
      ; ( "a second Unexpected does not displace the first"
        , let a = Unexpected Probe and b = Unexpected Division_by_zero in
          picks a [a; b]
        )
      ]
    ; List.map
        (fun (name, r) ->
          (describe "[%s] alone reports itself" name, picks r [r])
        )
        one_of_each
    ; pairs
    ; golden
      |> List.map (fun (name, reason, code, _) ->
          ( describe "%s's to_string starts with %s, so the log can be grepped"
              name code
          , String.starts_with ~prefix:(code ^ ": ") (to_string reason)
          )
      )
    ]

let checks =
  List.concat
    [
      golden
      |> List.concat_map (fun (name, reason, code, params) ->
          match to_api_error reason with
          | Api_errors.Server_error (actual_code, actual_params) ->
              [
                ( describe "%s projects to %s, not %s" name code actual_code
                , actual_code = code
                )
              ; ( describe "%s carries the payload [%s], not [%s]" name
                    (String.concat "; " params)
                    (String.concat "; " actual_params)
                , actual_params = params
                )
              ]
          | e ->
              [
                ( describe
                    "%s projects to an Api_errors.Server_error, not to %s" name
                    (Printexc.to_string e)
                , false
                )
              ]
      )
    ; golden
      |> List.map (fun (name, _, code, _) ->
          ( describe
              "%s projects to %s, which is not declared by add_error in \
               api_errors.ml, so no client would understand it"
              name code
          , is_declared code
          )
      )
    ; unexpected_cases
      |> List.map (fun (name, e) ->
          ( describe "Unexpected (%s) is returned unchanged" name
          , to_api_error (Unexpected e) = e
          )
      )
    ; golden
      |> List.concat_map (fun (name, reason, code, _) ->
          let s = to_string reason in
          [
            (describe "%s has a non-empty to_string" name, String.length s > 0)
          ; ( describe "%s's to_string is one line" name
            , not (String.contains s '\n')
            )
          ; ( describe "%s's to_string is prose for a log, not the bare code %s"
                name code
            , s <> code
            )
          ]
      )
    ; unexpected_cases
      |> List.map (fun (name, e) ->
          ( describe "Unexpected (%s) has a non-empty to_string" name
          , String.length (to_string (Unexpected e)) > 0
          )
      )
    ; best_refusal_checks
    ; [
        ( describe "the %d golden reasons have %d distinct to_string renderings"
            (List.length golden)
            (List.length
               (List.sort_uniq compare
                  (List.map (fun (_, r, _, _) -> to_string r) golden)
               )
            )
        , List.length
            (List.sort_uniq compare
               (List.map (fun (_, r, _, _) -> to_string r) golden)
            )
          = List.length golden
        )
      ]
    ]

(* Exit 2, not 1, when too few checks ran: an empty list would otherwise
   pass having compared nothing. *)
let () =
  let failures = List.filter (fun (_, ok) -> not ok) checks in
  List.iter (fun (name, _) -> print_endline ("  FAIL  " ^ name)) failures ;
  Printf.printf
    "\n%d checks, %d failed (%d reasons pinned, %d codes declared)\n"
    (List.length checks) (List.length failures) (List.length golden)
    (List.length declared) ;
  if golden = [] || declared = [] then (
    Printf.eprintf
      "BROKEN CHECK: %d reasons pinned and %d codes declared -- a list came \
       back empty, so this verified nothing\n"
      (List.length golden) (List.length declared) ;
    exit 2
  ) ;
  if List.length checks < 95 then (
    Printf.eprintf
      "VACUOUS: ran %d checks, expected at least 95 -- this verified less than \
       it should have\n"
      (List.length checks) ;
    exit 2
  ) ;
  exit (match failures with [] -> 0 | _ -> 1)
