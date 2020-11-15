open Angstrom

module P = struct
  let is_tab = function '\t' -> true | _ -> false

  let is_eol = function '\r' | '\n' -> true | _ -> false

  let is_unascii = function '\032' .. '\126' -> true | _ -> false
end

let selector = take_while P.is_unascii <* end_of_line <* end_of_input

let eol =  take_while P.is_eol <* end_of_input

let parse_request = eol <|> selector

type gopher_error = CommandNotUnderstood of string
[@@deriving show]

and request = Selection of string | Menu | GopherError of gopher_error
[@@deriving show]

let eval str =
  match parse_string ~consume:All parse_request str with
  | Ok v when v = "\n" -> Menu
  | Ok v -> Selection v
  | Error msg -> GopherError (CommandNotUnderstood msg)
