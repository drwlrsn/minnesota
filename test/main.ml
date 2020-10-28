(* let test expected () =
  Alcotest.(check unit) "test 0" expected (Main.main ())

let suite =
  [
    ("Dummy passing test", `Quick, test "Hello, World!");
    ("Dummy failing test", `Quick, test "Bye, World!");
  ]

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "minnesota" [ ("suite", suite) ] *)
