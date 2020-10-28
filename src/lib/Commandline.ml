open Cmdliner

let port =
  let doc = "The port the server will listen on." in
  Arg.(value & opt int 70 & info [ "p"; "port" ] ~docv:"<port>" ~doc)

let run port = print_endline @@ ( ^ ) "Running on port: " @@ string_of_int port

let run_t = Term.(const run $ port)

let info =
  let doc = "Start a gopher server" in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <me@drewlarson.ca>." ]
  in
  Term.info "minnesota" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man
