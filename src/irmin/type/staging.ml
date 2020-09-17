module Staged = struct
  type +'a t = 'a
end

type 'a staged = 'a Staged.t

let stage x = x

let unstage x = x
