type contents = Bytes of bytes | Text of string [@@deriving show]

module Contents : Irmin.Contents.S with type t = contents = struct
  type t = contents

  let t =
    let open Irmin_type.Type in
    Irmin_type.Type.variant "contents" (fun b s -> function
      | Bytes b' -> b b' | Text s' -> s s')
    |~ case1 "Bytes" bytes (fun b -> Bytes b)
    |~ case1 "Text" string (fun s -> Text s)
    |> sealv

  let merge = Irmin.Merge.(option (default t))
end

module Store (KV : Irmin.KV_MAKER) (C : Irmin.Contents.S) = struct
  open Lwt.Infix
  module R = KV (C)

  let add_file store name f =
    let info () =
      Irmin.Info.v ~date:0L ~author:"minnesota"
      @@ Printf.sprintf "added %s" name
    in
    R.set_exn store [ name ] f ~info

  let read_file store name = R.get store [ name ]

  let v f =
    let config = f () in
    R.Repo.v config >>= R.master
end

module Gopher_store = Store (Irmin_mem.KV) (Contents)

let make_store () = Gopher_store.v Irmin_mem.config
