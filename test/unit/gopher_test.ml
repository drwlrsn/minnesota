open Alcotest

let test_is_tab () =
  let msg = "expected true" in
  let expected = true in
  let actual = Minnesota.Gopher.P.is_tab '\t' in
  Alcotest.(check' bool) ~msg ~expected ~actual

let test_is_tab_fails () =
  let msg = "expected false" in
  let expected = false in
  let actual = Minnesota.Gopher.P.is_tab '\n' in
  Alcotest.(check' bool) ~msg ~expected ~actual

let test_is_eol_with_n () =
  let msg = "expected true" in
  let expected = true in
  let actual = Minnesota.Gopher.P.is_eol '\n' in
  Alcotest.(check' bool) ~msg ~expected ~actual

let test_is_eol_with_r () =
  let msg = "expected true" in
  let expected = true in
  let actual = Minnesota.Gopher.P.is_eol '\r' in
  Alcotest.(check' bool) ~msg ~expected ~actual

let test_is_eol_with_other_char () =
  let msg = "expected false" in
  let expected = false in
  let actual = Minnesota.Gopher.P.is_eol 'k' in
  Alcotest.(check' bool) ~msg ~expected ~actual

let gopher_result = Alcotest.testable Minnesota.Gopher.pp_request ( = )

let test_parse_retrieval () =
  let msg = "expected Retrieval" in
  let expected = Minnesota.Gopher.(Selection "Some thing") in
  let actual = Minnesota.Gopher.(eval "Some thing\n") in
  Alcotest.(check' gopher_result) ~msg ~expected ~actual

let test_parse_list () =
  let msg = "expected List" in
  let expected = Minnesota.Gopher.(Menu) in
  let actual = Minnesota.Gopher.(eval "\n") in
  Alcotest.(check' gopher_result) ~msg ~expected ~actual

let tests =
  [
    test_case "Char is a tab" `Quick test_is_tab;
    test_case "Char is not a tab" `Quick test_is_tab_fails;
    test_case "Char is a \\n" `Quick test_is_eol_with_n;
    test_case "Char is a \\r" `Quick test_is_eol_with_r;
    test_case "Char is a k" `Quick test_is_eol_with_other_char;
    test_case "Parse list" `Quick test_parse_list;
    test_case "Parse retrieval" `Quick test_parse_retrieval;
  ]
