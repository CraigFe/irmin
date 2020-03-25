module type S = sig
  type commit
  type branch
  type slice

  (** {1 Repositories}

      A repository contains a set of branches. *)

  type t
  (** The type of repository handles. *)

  val v : S.config -> t Lwt.t
  (** [v config] connects to a repository in a backend-specific manner. *)

  val close : t -> unit Lwt.t
  (** [close t] frees up all resources associated with [t]. Any operations run
      on a closed repository will raise {!Closed}. *)

  val heads : t -> commit list Lwt.t
  (** [heads] is {!Head.list}. *)

  val branches : t -> branch list Lwt.t
  (** [branches] is {!Branch.list}. *)

  val export :
    ?full:bool ->
    ?depth:int ->
    ?min:commit list ->
    ?max:[ `Head | `Max of commit list ] ->
    t ->
    slice Lwt.t
  (** [export t ~full ~depth ~min ~max] exports the store slice between [min]
      and [max], using at most [depth] history depth (starting from the max).

      If [max] is `Head (also the default value), use the current [heads]. If
      [min] is not specified, use an unbound past (but can still be limited by
      [depth]).

      [depth] is used to limit the depth of the commit history. [None] here
      means no limitation.

      If [full] is set (default is true), the full graph, including the commits,
      nodes and contents, is exported, otherwise it is the commit history graph
      only. *)

  val import : t -> slice -> (unit, [ `Msg of string ]) result Lwt.t
  (** [import t s] imports the contents of the slice [s] in [t]. Does not modify
      branches. *)
end

module type MAKER = functor (_ : Logs.LOG) (P : S.PRIVATE) ->
  S with type t = P.Repo.t and type slice = P.Slice.t and type commit = Store_commit.Make(P).t

module type Repo = sig
  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
