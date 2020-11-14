

let () =
  let open Alcotest in
  run "Parse Utils"
    [
      ( "char-predicate",
        List.concat
          [
            Unit.tests; 
            Property.tests;
          ] );
    ]
