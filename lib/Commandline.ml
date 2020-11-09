open Cmdliner

let port =
  let doc = "The port the server will listen on." in
  Arg.(value & opt int 70 & info [ "p"; "port" ] ~docv:"<port>" ~doc)

let host =
  let doc = "The host the server will listen on." in
  Arg.(
    value & opt string "127.0.0.1" & info [ "h"; "host" ] ~docv:"<host>" ~doc)

let info =
  let doc = "Start a gopher server" in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <me@drewlarson.ca>." ]
  in
  Term.info "minnesota" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits
    ~man
