open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let main port host style_renderer level =
  setup_log style_renderer level;
  Lwt_main.run (Minnesota.Server.fiber ~port ~host)

let run_t =
  Term.(
    const main $ Minnesota.Commandline.port $ Minnesota.Commandline.host
    $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let () = Term.exit @@ Term.eval (run_t, Minnesota.Commandline.info)
