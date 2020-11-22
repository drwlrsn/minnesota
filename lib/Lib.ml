
module Commandline = Commandline
module Server = Server
module Gopher = struct
  include Gopher

  module P = Gopher.P
end
module Fs = Fs