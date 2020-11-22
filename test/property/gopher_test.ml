let to_alcotest_sync test =
  let name, s, run = QCheck_alcotest.to_alcotest test in
  (name, s, fun () -> Lwt.return @@ run ())

let unascii_gen =
  QCheck.make
    ~print:(fun c -> Printf.sprintf "%c code: %d" c @@ Char.code c)
    QCheck.Gen.(char_range '\040' '\126')

let test_parse =
  QCheck.Test.make ~count:1000 ~name:"Parses strings"
    QCheck.(
      string_gen
        QCheck.Gen.(char_range '\040' '\126' >>= fun _ -> char_range '\r' '\r'))
    (fun s ->
      match Minnesota.Gopher.eval s with
      | Minnesota.Gopher.Menu -> true
      | Minnesota.Gopher.Selection _ -> true
      | Minnesota.Gopher.GopherError _ -> false)

let test_is_unascii =
  QCheck.Test.make ~count:1000 ~name:"Handles ascii chars" unascii_gen
    Minnesota__Gopher.P.is_unascii

let tests = List.map to_alcotest_sync [ test_parse; test_is_unascii ]
