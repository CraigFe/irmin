(** This module is intended to be globally opened. *)

module Staged : sig
  type +'a t
end

type +'a staged = 'a Staged.t

val stage : 'a -> 'a staged

val unstage : 'a staged -> 'a
