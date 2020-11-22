open Angstrom

module GopherMapItem = struct
  type t = {
    document_type : document_types;
    display_name : display_name;
    fqdn : fqdn;
    path : path;
    port : port;
  }

  and document_types =
    | Text of char
    | Submenu of char
    | Nameserver of char
    | Error of char
    | BinHex of char
    | Dos of char
    | UUEncoded of char
    | Search of char
    | Telnet of char
    | Binary of char
    | Mirror of char
    | Gif of char
    | Image of char
    | Telnet3270 of char

  and display_name = string

  and fqdn = { host : string option; domain : string; tld : string option }

  and path = string

  and port = int32

  let document_type_to_char = function
    | Text c -> c
    | Submenu c -> c
    | Nameserver c -> c
    | Error c -> c
    | BinHex c -> c
    | Dos c -> c
    | UUEncoded c -> c
    | Search c -> c
    | Telnet c -> c
    | Binary c -> c
    | Mirror c -> c
    | Gif c -> c
    | Image c -> c
    | Telnet3270 c -> c

  let make_document_type = function
    | `Text -> Text '0'
    | `Submenu -> Submenu '1'
    | `Nameserver -> Nameserver '2'
    | `Error -> Error '3'
    | `BinHex -> BinHex '4'
    | `Dos -> Dos '5'
    | `UUEncoded -> UUEncoded '6'
    | `Search -> Search '7'
    | `Telnet -> Telnet '8'
    | `Binary -> Binary '9'
    | `Mirror -> Mirror '+'
    | `Gif -> Gif 'g'
    | `Image -> Image 'i'
    | `Telnet3270 -> Telnet3270 't'

  let make_fqdn ?host ?tld domain = { host; domain; tld }

  let fqdn_to_string { host; domain; tld } =
    match (host, tld, domain) with
    | None, None, d -> d
    | None, Some t, d -> d ^ "." ^ t
    | Some h, Some t, d -> h ^ "." ^ d ^ "." ^ t
    | Some h, None, d -> h ^ "." ^ d

  let port_to_string = Int32.to_string

  let make_item document_type display_name fqdn path port =
    { document_type; fqdn; path; port; display_name }

  let to_string { document_type; fqdn; path; port; display_name } =
    let document_type' = document_type_to_char document_type |> Char.escaped in
    let fqdn' = fqdn |> fqdn_to_string in
    let port' = port |> port_to_string in
    Format.sprintf "%s%s\t%s\t%s\t%s\r\n" document_type' display_name path fqdn'
      port'
end

module GopherMap = struct
  type t = GopherMapItem.t list

  let to_string gmap =
    gmap
    |> List.map GopherMapItem.to_string
    |> String.concat "\n" |> Fun.flip ( ^ ) "\n."
end

module P = struct
  let is_tab = function '\t' -> true | _ -> false

  let is_eol = function '\r' | '\n' -> true | _ -> false

  let is_unascii = function '\032' .. '\126' -> true | _ -> false
end

let selector = take_while P.is_unascii <* end_of_line <* end_of_input

let eol = take_while P.is_eol <* end_of_input

let parse_request = eol <|> selector

type gopher_error = CommandNotUnderstood of string [@@deriving show]

and request = Selection of string | Menu | GopherError of gopher_error
[@@deriving show]

let eval str =
  match parse_string ~consume:All parse_request str with
  | Ok v when v = "\r\n" -> Menu
  | Ok v -> Selection v
  | Error msg -> GopherError (CommandNotUnderstood msg)

let fake_fqdn = GopherMapItem.make_fqdn "localhost"

let fake_menu =
  GopherMap.to_string
    [
      GopherMapItem.make_item
        (GopherMapItem.make_document_type `Binary)
        "cool thing.exe" fake_fqdn "path" (Int32.of_int 70);
    ]

let handle request =
  let explode s = List.init (String.length s) (String.get s) in
  let implode sep strings =
    List.fold_left (fun str acc -> str ^ sep ^ acc) "" strings
  in
  Logs.debug (fun m ->
      m "Received request: %S as codes: %S" request
        ( explode request |> List.map Char.code |> List.map Int.to_string
        |> implode " ," ));
  match eval request with
  | Menu -> fake_menu |> Bytes.of_string
  | _ -> "Whoops not implemented" |> Bytes.of_string
