(** Tests the presence of last-known-good runtime files. *)

open Tiger.Driver

let test_lkg kind =
  let lkg = lkg_runtime kind in
  let test _ =
    if not (Sys.file_exists (Filename.concat ".." lkg)) then
      Alcotest.fail (Printf.sprintf "LKG %s missing" lkg)
  in
  (lkg, `Quick, test)

let () =
  Alcotest.run "runtime tests"
    [("lkg", List.map test_lkg [X86_64_apple_darwin20_1_0])]
