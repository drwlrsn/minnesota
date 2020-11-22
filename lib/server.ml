open Lwt.Infix

(* 4096 *)
let capacity = 0x1000

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error err -> Lwt.return (Error err)

(* See for ideas https://github.com/dune-universe/dune-universe/blob/07ef20ca81afb87e97049880dc27c712953ff8b9/packages/conduit.3.0.0/src/core/howto.mld *)
(* https://gist.github.com/dinosaure/ac2a5956a16370ddcf965a1980f5c9a0 *)
(* let config =
  { Conduit_lwt.TCP.sockaddr = Lwt_unix.ADDR_UNIX "localhost"; capacity = 1 } *)

let getline queue =
  let exists ~predicate queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter
      (fun chr ->
        if predicate chr then res := !pos;
        Logs.debug (fun m -> m "Found %C at %d" chr !pos);
        incr pos)
      queue;
    if !res = -1 then None else Some !res
  in
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len
  in
  match exists ~predicate:(( = ) '\n') queue with
  | Some pos ->
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 tmp;
      Ke.Rke.N.shift_exn queue (pos + 1);
      Some (Bytes.unsafe_to_string tmp ^ "\n")
  | None -> None

let getline queue flow =
  let tmp = Cstruct.create capacity in
  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len
  in
  let rec go () =
    match getline queue with
    | Some line -> Lwt.return_ok (`Line line)
    | None -> (
        Conduit_lwt.recv flow tmp >>? function
        | `End_of_flow as r -> Lwt.return_ok r
        | `Input len ->
            Ke.Rke.N.push queue ~blit ~length:Cstruct.len ~off:0 ~len tmp;
            go () )
  in
  go ()

let handler flow =
  let queue = Ke.Rke.create ~capacity Bigarray.char in
let go () =
    getline queue flow >>= function
    | Ok `End_of_flow | Error _ -> Conduit_lwt.close flow
    (* | Ok (`Line "ping") ->
           Conduit_lwt.send flow (Cstruct.of_string "pong\n") >>? fun _ -> go ()
       | Ok (`Line "pong") ->
           Conduit_lwt.send flow (Cstruct.of_string "ping\n") >>? fun _ -> go () *)
    | Ok (`Line line) ->
        Conduit_lwt.send flow (Cstruct.of_bytes @@ Gopher.handle line) |> ignore;
        Conduit_lwt.close flow
        (* >>? fun _ -> go () *)
  in
  go () >>= function
  | Error err -> Fmt.failwith "%a" Conduit_lwt.pp_error err
  | Ok () -> Lwt.return_unit

let server cfg ~protocol ~service =
  Logs.info (fun m -> m "Starting server...");
  Conduit_lwt.serve
    ~handler:(fun flow -> handler (Conduit_lwt.pack protocol flow))
    ~service cfg

let fiber ~host ~port =
  let cfg : Conduit_lwt.TCP.configuration =
    {
      Conduit_lwt.TCP.sockaddr =
        Unix.(ADDR_INET (inet_addr_of_string host, port));
      capacity = 40;
    }
  in
  let _always, run =
    server cfg ~protocol:Conduit_lwt.TCP.protocol
      ~service:Conduit_lwt.TCP.service
  in
  Logs.info (fun m -> m "Listening on %s:%d" host port);
  run ()
