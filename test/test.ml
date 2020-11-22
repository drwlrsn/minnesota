let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Minnesota"
       [
         ("char-predicate", List.concat [ Unit.Gopher.tests; Property.tests ]);
         ("store", Unit.Fs.tests);
       ]
