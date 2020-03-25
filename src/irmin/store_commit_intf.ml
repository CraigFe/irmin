module Sigs = S

module type S = sig
  type repo

  type hash

  type t
  (** The type for store commits. *)

  val t : repo -> t Type.t
  (** [t] is the value type for {!t}. *)

  val pp_hash : t Fmt.t
  (** [pp] is the pretty-printer for commit. Display only the hash. *)

  val parents : t -> hash list
  (** [parents c] are [c]'s parents. *)

  val info : t -> Info.t
  (** [info c] is [c]'s info. *)

  (** {1 Import/Export} *)

  val hash : t -> hash
  (** [hash c] it [c]'s hash. *)

  val of_hash : repo -> hash -> t option Lwt.t
  (** [of_hash r h] is the the commit object in [r] having [h] as hash, or
      [None] is no such commit object exists. *)
end

module type MAKER = functor
  (P : Sigs.PRIVATE)
  -> sig
  include S

  val equal : t -> t -> bool

  val equal_opt : t option -> t option -> bool

  val node : t -> hash

  val to_private_commit : t -> P.Commit.value

  val of_private_commit : P.Repo.t -> P.Commit.value -> t
end
with type repo := P.Repo.t
 and type hash := P.Hash.t

module type Store_commit = sig
  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
