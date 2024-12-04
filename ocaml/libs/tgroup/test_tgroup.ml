module D = Debug.Make (struct let name = __MODULE__ end)

let test_identity () =
  let user_agents_and_sids =
    [
      (Some "XenCenter2024", "u1000")
    ; (None, "u1001")
    ; (None, "Special!@#")
    ; (Some "With-Hyphen", "123")
    ; (Some "", "")
    ; (Some " Xen Center 2024 ", ", u 1000 ")
    ; (Some "Xen Center ,/@.~# 2024", "root")
    ; (Some "XenCenter 2024.3.18", "")
    ]
  in
  let expected_identities =
    [
      "u1000/XenCenter2024"
    ; "u1001"
    ; "Special"
    ; "123/WithHyphen"
    ; "root"
    ; "u1000/XenCenter2024"
    ; "root/XenCenter2024"
    ; "root/XenCenter2024318"
    ]
  in
  let test_make (user_agent, subject_sid) identity =
    let actual_identity =
      Tgroup.Group.Identity.(make ?user_agent subject_sid |> to_string)
    in
    Alcotest.(check string) "Check expected identity" identity actual_identity
  in
  List.iter2 test_make user_agents_and_sids expected_identities

let test_of_creator () =
  let dummy_identity =
    Tgroup.Group.Identity.make ~user_agent:"XenCenter2024" "root"
  in
  let creator_args =
    [
      (None, None, None, None)
    ; (Some true, None, None, None)
    ; (Some true, Some "external", Some dummy_identity, Some "sm")
    ; (Some true, Some "internal", Some dummy_identity, Some "sm")
    ; (None, Some "intenal", Some dummy_identity, Some "cli")
    ; (None, None, Some dummy_identity, Some "sm")
    ]
  in
  let expected_groups =
    [
      "external/unauthenticated"
    ; "external/intrapool"
    ; "external/intrapool"
    ; "external/intrapool"
    ; "internal/cli"
    ; "external/authenticated/root/XenCenter2024"
    ]
  in
  let test_make (intrapool, endpoint, identity, originator) group =
    let actual_group =
      Tgroup.Group.(
        Creator.make ?intrapool ?endpoint ?identity ?originator ()
        |> of_creator
        |> to_string
      )
    in
    Alcotest.(check string) "Check expected group" group actual_group
  in
  List.iter2 test_make creator_args expected_groups

let tests =
  [
    ("Identity make", `Quick, test_identity)
  ; ("group of creator", `Quick, test_of_creator)
  ]

let () = Alcotest.run "Tgroup library" [("Thread classification", tests)]
