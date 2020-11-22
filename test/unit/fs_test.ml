open Lwt.Syntax

let contents_testable = Alcotest.testable Minnesota.Fs.pp_contents ( = )

let test_store_text _ () =
  let open Minnesota.Fs in
  let msg = "expected get text" in
  let store = make_store () in
  let expected = Text "Oh wow" in
  let _ =
    let* s = store in
    Gopher_store.add_file s "Cool file" (Text "Oh wow")
  in
  let v =
    let* s = store in
    Gopher_store.read_file s "Cool file"
  in
  let* actual = v in
  Lwt.return @@ Alcotest.(check' contents_testable) ~msg ~expected ~actual

let get_file name =
  let buffer = Bytes.create @@ (100 * 1024) in
  let ic = open_in_bin name in
  let rec go pos buf =
    let buf' =
      if pos >= Bytes.length buf then Bytes.extend buf 0 1024 else buf
    in
    let a = input ic buffer pos 1 in
    if a > 0 then go (Int.succ pos) buf' else buf'
  in

  try go 0 buffer
  with e ->
    close_in_noerr ic;
    raise e

let test_store_bin _ () =
  let open Minnesota.Fs in
  let msg = "expected get bytes" in
  let store = make_store () in
  let expected =
    Minnesota.Fs.Bytes
      (get_file @@ Unix.getcwd () ^ "/../../../test/unit/data/test.jpg")
  in
  let _ =
    let* s = store in
    Gopher_store.add_file s "test.jpg"
    @@ Minnesota.Fs.Bytes
         (get_file @@ Unix.getcwd () ^ "/../../../test/unit/data/test.jpg")
  in
  let v =
    let* s = store in
    Gopher_store.read_file s "test.jpg"
  in
  let* actual = v in
  Lwt.return @@ Alcotest.(check' contents_testable) ~msg ~expected ~actual

let tests =
  [
    Alcotest_lwt.test_case "Add text and retrieve text" `Quick test_store_text;
    Alcotest_lwt.test_case "Add image and retrieve image" `Slow test_store_bin;
  ]
