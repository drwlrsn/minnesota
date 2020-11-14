open Cmdliner

let main port host = Lwt_main.run (Minnesota.Server.fiber ~port ~host)

let run_t =
  Term.(const main $ Minnesota.Commandline.port $ Minnesota.Commandline.host)

let () = Term.exit @@ Term.eval (run_t, Minnesota.Commandline.info)
