open Cmdliner

let () = Term.exit @@ Term.eval (Lib.Commandline.run_t, Lib.Commandline.info)

