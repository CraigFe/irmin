(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Irmin signatures *)

module type PATH = sig
  (** {1 Path} *)

  type t
  (** The type for path values. *)

  type step
  (** Type type for path's steps. *)

  val empty : t
  (** The empty path. *)

  val v : step list -> t
  (** Create a path from a list of steps. *)

  val is_empty : t -> bool
  (** Check if the path is empty. *)

  val cons : step -> t -> t
  (** Prepend a step to the path. *)

  val rcons : t -> step -> t
  (** Append a step to the path. *)

  val decons : t -> (step * t) option
  (** Deconstruct the first element of the path. Return [None] if
        the path is empty. *)

  val rdecons : t -> (t * step) option
  (** Deconstruct the last element of the path. Return [None] if the
        path is empty. *)

  val map : t -> (step -> 'a) -> 'a list
  (** [map t f] maps [f] over all steps of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)
end

module type HASH = sig
  (** Signature for digest hashes, inspired by Digestif. *)

  type t
  (** The type for digest hashes. *)

  val hash : ((string -> unit) -> unit) -> t
  (** Compute a deterministic store key from a sequence of strings. *)

  val short_hash : t -> int
  (** [short_hash h] is a small hash of [h], to be used for instance as
       the `hash` function of an OCaml [Hashtbl]. *)

  val hash_size : int
  (** [hash_size] is the size of hash results, in bytes. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)
end

module type TYPED_HASH = sig
  type t

  type value

  val hash : value -> t
  (** Compute a deterministic store key from a string. *)

  val short_hash : t -> int
  (** [short_hash h] is a small hash of [h], to be used for instance as
       the `hash` function of an OCaml [Hashtbl]. *)

  val hash_size : int
  (** [hash_size] is the size of hash results, in bytes. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)
end

module type CONTENTS = sig
  (** {1 Signature for store contents} *)

  type t
  (** The type for user-defined contents. *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val merge : t option Merge.t
  (** Merge function. Evaluates to [`Conflict msg] if the values
      cannot be merged properly. The arguments of the merge function
      can take [None] to mean that the key does not exists for
      either the least-common ancestor or one of the two merging
      points. The merge function returns [None] when the key's value
      should be deleted. *)
end

module type CONTENT_ADDRESSABLE_STORE = sig
  (** {1 Content-addressable stores}

      Content-addressable stores are store where it is possible to read
      and add new values. Keys are derived from the values raw contents
      and hence are deterministic. *)

  type 'a t
  (** The type for content-addressable backend stores. The ['a]
      phantom type carries information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> `Read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> `Read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and
      [None] is [k] is not present in [t]. *)

  val add : [> `Write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the
      responsibility of the content-addressable store to generate a
      consistent key. *)

  val unsafe_add : [> `Write ] t -> key -> value -> unit Lwt.t
  (** Same as {!add} but allows to specify the key directly. The
      backend might choose to discared that key and/or can be corrupt
      if the key scheme is not consistent. *)
end

module type CONTENT_ADDRESSABLE_STORE_MAKER = functor
  (K : HASH)
  (V : Type.S)
  -> sig
  include CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The
     exact guarantees depends on the backends. *)

  val v : Conf.t -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise {!Closed}. *)
end

module type APPEND_ONLY_STORE = sig
  (** {1 Append-only stores}

      Append-onlye stores are store where it is possible to read
      and add new values. *)

  type 'a t
  (** The type for append-only backend stores. The ['a]
     phantom type carries information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> `Read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> `Read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and
      [None] is [k] is not present in [t]. *)

  val add : [> `Write ] t -> key -> value -> unit Lwt.t
  (** Write the contents of a value to the store. *)
end

module type APPEND_ONLY_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The
     exact guarantees depends on the backends. *)

  val v : Conf.t -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise {!Closed}. *)
end

module type METADATA = sig
  type t
  (** The type for metadata. *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val merge : t Merge.t
  (** [merge] is the merge function for metadata. *)

  val default : t
  (** The default metadata to attach, for APIs that don't
      care about metadata. *)
end

module type CONTENTS_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  val merge : [ `Read | `Write ] t -> key option Merge.t

  module Key : TYPED_HASH with type t = key and type value = value

  module Val : CONTENTS with type t = value
end

module type NODE = sig
  (** {1 Node values} *)

  type t
  (** The type for node values. *)

  type metadata
  (** The type for node metadata. *)

  type hash
  (** The type for keys. *)

  type step
  (** The type for steps between nodes. *)

  type value = [ `Node of hash | `Contents of hash * metadata ]
  (** The type for either (node) keys or (contents) keys combined with
      their metadata. *)

  val v : (step * value) list -> t
  (** [create l] is a new node. *)

  val list : t -> (step * value) list
  (** [list t] is the contents of [t]. *)

  val empty : t
  (** [empty] is the empty node. *)

  val is_empty : t -> bool
  (** [is_empty t] is true iff [t] is {!empty}. *)

  val find : t -> step -> value option
  (** [find t s] is the value associated with [s] in [t].

      A node can point to user-defined
      {{!Node.S.contents}contents}. The edge between the node and
      the contents is labeled by a {{!Node.S.step}step}. *)

  val add : t -> step -> value -> t
  (** [add t s v] is the node where [find t v] is [Some s] but
      is similar to [t] otherwise. *)

  val remove : t -> step -> t
  (** [remove t s] is the node where [find t s] is [None] but is
      similar to [t] otherwise. *)

  (** {1 Value types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val default : metadata
  (** [default] is the default metadata value. *)

  val metadata_t : metadata Type.t
  (** [metadata_t] is the value type for {!metadata}. *)

  val hash_t : hash Type.t
  (** [hash_t] is the value type for {!hash}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type NODE_GRAPH = sig
  (** {1 Node Graphs} *)

  type 'a t
  (** The type for store handles. *)

  type metadata
  (** The type for node metadata. *)

  type contents
  (** The type of user-defined contents. *)

  type node
  (** The type for node values. *)

  type step
  (** The type of steps. A step is used to pass from one node to
      another. *)

  type path
  (** The type of store paths. A path is composed of
      {{!step}steps}. *)

  type value = [ `Node of node | `Contents of contents * metadata ]
  (** The type for store values. *)

  val empty : [> `Write ] t -> node Lwt.t
  (** The empty node. *)

  val v : [> `Write ] t -> (step * value) list -> node Lwt.t
  (** [v t n] is a new node containing [n]. *)

  val list : [> `Read ] t -> node -> (step * value) list Lwt.t
  (** [list t n] is the contents of the node [n]. *)

  val find : [> `Read ] t -> node -> path -> value option Lwt.t
  (** [find t n p] is the contents of the path [p] starting form
      [n]. *)

  val add : [ `Read | `Write ] t -> node -> path -> value -> node Lwt.t
  (** [add t n p v] is the node [x] such that [find t x p] is
      [Some v] and it behaves the same [n] for other
      operations. *)

  val remove : [ `Read | `Write ] t -> node -> path -> node Lwt.t
  (** [remove t n path] is the node [x] such that [find t x] is
      [None] and it behhaves then same as [n] for other
      operations. *)

  val closure :
    [> `Read ] t -> min:node list -> max:node list -> node list Lwt.t
  (** [closure t ~min ~max] is the transitive closure [c] of [t]'s
      nodes such that:

      {ul
      {- There is a path in [t] from any nodes in [min] to nodes
      in [c]. If [min] is empty, that condition is always true.}
      {- There is a path in [t] from any nodes in [c] to nodes in
      [max]. If [max] is empty, that condition is always false.}
      }

      {b Note:} Both [min] and [max] are subsets of [c].*)

  val iter_on_closure :
    [> `Read ] t ->
    min:node list ->
    max:node list ->
    f_nodes:(node -> unit Lwt.t) ->
    f_edges:(node -> node list -> unit Lwt.t) ->
    unit Lwt.t
  (** [iter_on_closure min max pred f_nodes f_edges ()] is the same as
      closure except that it applies [f_nodes] on the nodes and [f_edges] on
      the edges of the closure graph while traversing it. *)

  (** {1 Value Types} *)

  val metadata_t : metadata Type.t
  (** [metadat_t] is the value type for {!metadata}. *)

  val contents_t : contents Type.t
  (** [contents_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val path_t : path Type.t
  (** [path_t] is the value type for {!path}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type NODE_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  module Path : PATH

  val merge : [ `Read | `Write ] t -> key option Merge.t

  module Key : TYPED_HASH with type t = key and type value = value

  module Metadata : METADATA

  module Val :
    NODE
      with type t = value
       and type hash = key
       and type metadata = Metadata.t
       and type step = Path.step

  module Contents : CONTENTS_STORE with type key = Val.hash
end

type config = Conf.t

type 'a diff = 'a Diff.t

module type COMMIT = sig
  (** {1 Commit values} *)

  type t
  (** The type for commit values. *)

  type hash
  (** Type for keys. *)

  val v : info:Info.t -> node:hash -> parents:hash list -> t
  (** Create a commit. *)

  val node : t -> hash
  (** The underlying node. *)

  val parents : t -> hash list
  (** The commit parents. *)

  val info : t -> Info.t
  (** The commit info. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val hash_t : hash Type.t
  (** [hash_t] is the value type for {!hash}. *)
end

module type COMMIT_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  val merge : [ `Read | `Write ] t -> info:Info.f -> key option Merge.t

  module Key : TYPED_HASH with type t = key and type value = value

  module Val : COMMIT with type t = value and type hash = key

  module Node : NODE_STORE with type key = Val.hash
end

module type COMMIT_HISTORY = sig
  (** {1 Commit History} *)

  type 'a t
  (** The type for store handles. *)

  type node
  (** The type for node values. *)

  type commit
  (** The type for commit values. *)

  type v
  (** The type for commit objects. *)

  val v :
    [> `Write ] t ->
    node:node ->
    parents:commit list ->
    info:Info.t ->
    (commit * v) Lwt.t
  (** Create a new commit. *)

  val parents : [> `Read ] t -> commit -> commit list Lwt.t
  (** Get the commit parents.

      Commits form a append-only, fully functional, partial-order
      data-structure: every commit carries the list of its
      immediate predecessors. *)

  val merge : [ `Read | `Write ] t -> info:Info.f -> commit Merge.t
  (** [merge t] is the 3-way merge function for commit.  *)

  val lcas :
    [> `Read ] t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit list, [ `Max_depth_reached | `Too_many_lcas ]) result Lwt.t
  (** Find the lowest common ancestors
      {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor}lca}
      between two commits. *)

  val lca :
    [ `Read | `Write ] t ->
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit list ->
    (commit option, Merge.conflict) result Lwt.t
  (** Compute the lowest common ancestors ancestor of a list of
      commits by recursively calling {!lcas} and merging the
      results.

      If one of the merges results in a conflict, or if a call to
      {!lcas} returns either [Error `Max_depth_reached] or
      [Error `Too_many_lcas] then the function returns the same
      error. *)

  val three_way_merge :
    [ `Read | `Write ] t ->
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit, Merge.conflict) result Lwt.t
  (** Compute the {!lcas} of the two commit and 3-way merge the
      result. *)

  val closure :
    [> `Read ] t -> min:commit list -> max:commit list -> commit list Lwt.t
  (** Same as {{!Private.Node.GRAPH.closure}GRAPH.closure} but for
      the history graph. *)

  (** {1 Value Types} *)

  val commit_t : commit Type.t
  (** [commit_t] is the value type for {!commit}. *)
end

module type SLICE = sig
  (** {1 Slices} *)

  type t
  (** The type for slices. *)

  type contents
  (** The type for exported contents. *)

  type node
  (** The type for exported nodes. *)

  type commit
  (** The type for exported commits. *)

  type value = [ `Contents of contents | `Node of node | `Commit of commit ]
  (** The type for exported values. *)

  val empty : unit -> t Lwt.t
  (** Create a new empty slice. *)

  val add : t -> value -> unit Lwt.t
  (** [add t v] adds [v] to [t]. *)

  val iter : t -> (value -> unit Lwt.t) -> unit Lwt.t
  (** [iter t f] calls [f] on all values of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val contents_t : contents Type.t
  (** [content_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val commit_t : commit Type.t
  (** [commit_t] is the value type for {!commit}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type BRANCH = sig
  include Type.S

  val master : t

  val is_valid : t -> bool
end

module type ATOMIC_WRITE_STORE = sig
  (** {1 Atomic write stores}

      Atomic-write stores are stores where it is possible to read,
      update and remove elements, with atomically guarantees. *)

  type t
  (** The type for atomic-write backend stores.  *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and
      [None] is [k] is not present in [t]. *)

  val set : t -> key -> value -> unit Lwt.t
  (** [set t k v] replaces the contents of [k] by [v] in [t]. If [k]
      is not already defined in [t], create a fresh binding.  Raise
      [Invalid_argument] if [k] is the {{!Path.empty}empty path}. *)

  val test_and_set :
    t -> key -> test:value option -> set:value option -> bool Lwt.t
  (** [test_and_set t key ~test ~set] sets [key] to [set] only if
      the current value of [key] is [test] and in that case returns
      [true]. If the current value of [key] is different, it returns
      [false]. [None] means that the value does not have to exist or
      is removed.

      {b Note:} The operation is guaranteed to be atomic. *)

  val remove : t -> key -> unit Lwt.t
  (** [remove t k] remove the key [k] in [t]. *)

  val list : t -> key list Lwt.t
  (** [list t] it the list of keys in [t]. *)

  type watch
  (** The type of watch handlers. *)

  val watch :
    t ->
    ?init:(key * value) list ->
    (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch t ?init f] adds [f] to the list of [t]'s watch handlers
      and returns the watch handler to be used with {!unwatch}. [init]
      is the optional initial values. It is more efficient to use
      {!watch_key} to watch only a single given key.*)

  val watch_key :
    t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch_key t k ?init f] adds [f] to the list of [t]'s watch
      handlers for the key [k] and returns the watch handler to be
      used with {!unwatch}. [init] is the optional initial value of
      the key. *)

  val unwatch : t -> watch -> unit Lwt.t
  (** [unwatch t w] removes [w] from [t]'s watch handlers. *)

  val close : t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any
      operations run on a closed store will raise {!Closed}. *)
end

module type ATOMIC_WRITE_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : Conf.t -> t Lwt.t
end

module type BRANCH_STORE = sig
  include ATOMIC_WRITE_STORE

  module Key : BRANCH with type t = key

  module Val : HASH with type t = value
end

type remote = ..

module type SYNC = sig
  (** {1 Remote synchronization} *)

  type t
  (** The type for store handles. *)

  type commit
  (** The type for store heads. *)

  type branch
  (** The type for branch IDs. *)

  type endpoint
  (** The type for sync endpoints. *)

  val fetch :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (commit option, [ `Msg of string ]) result Lwt.t
  (** [fetch t uri] fetches the contents of the remote store
      located at [uri] into the local store [t]. Return the head
      of the remote branch with the same name, which is now in the
      local store. [No_head] means no such branch exists. *)

  val push :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (unit, [ `Msg of string | `Detached_head ]) result Lwt.t
  (** [push t uri] pushes the contents of the local store [t] into
      the remote store located at [uri]. *)
end

module type PRIVATE = sig
  module Hash : HASH

  module Contents : CONTENTS_STORE with type key = Hash.t

  module Node :
    NODE_STORE with type key = Hash.t and type Val.hash = Contents.key

  module Commit :
    COMMIT_STORE with type key = Hash.t and type Val.hash = Node.key

  module Branch : BRANCH_STORE with type value = Commit.key

  module Slice :
    SLICE
      with type contents = Contents.key * Contents.value
       and type node = Node.key * Node.value
       and type commit = Commit.key * Commit.value

  module Repo : sig
    type t

    val v : Conf.t -> t Lwt.t

    val close : t -> unit Lwt.t

    val contents_t : t -> [ `Read ] Contents.t

    val node_t : t -> [ `Read ] Node.t

    val commit_t : t -> [ `Read ] Commit.t

    val branch_t : t -> Branch.t

    val batch :
      t ->
      ([ `Read | `Write ] Contents.t ->
      [ `Read | `Write ] Node.t ->
      [ `Read | `Write ] Commit.t ->
      'a Lwt.t) ->
      'a Lwt.t
  end

  module Sync : sig
    include SYNC with type commit = Commit.key and type branch = Branch.key

    val v : Repo.t -> t Lwt.t
  end
end

module type TREE = sig
  type key

  type step

  type metadata

  type contents

  type node

  type tree = [ `Node of node | `Contents of contents * metadata ]

  val empty : tree

  val of_contents : ?metadata:metadata -> contents -> tree

  val of_node : node -> tree

  val kind : tree -> key -> [ `Contents | `Node ] option Lwt.t

  val list : tree -> key -> (step * [ `Contents | `Node ]) list Lwt.t

  val diff : tree -> tree -> (key * (contents * metadata) diff) list Lwt.t

  val mem : tree -> key -> bool Lwt.t

  val find_all : tree -> key -> (contents * metadata) option Lwt.t

  val find : tree -> key -> contents option Lwt.t

  val get_all : tree -> key -> (contents * metadata) Lwt.t

  val get : tree -> key -> contents Lwt.t

  val add : tree -> key -> ?metadata:metadata -> contents -> tree Lwt.t

  val remove : tree -> key -> tree Lwt.t

  val mem_tree : tree -> key -> bool Lwt.t

  val find_tree : tree -> key -> tree option Lwt.t

  val get_tree : tree -> key -> tree Lwt.t

  val add_tree : tree -> key -> tree -> tree Lwt.t

  val merge : tree Merge.t

  type marks

  val empty_marks : unit -> marks

  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]

  type uniq = [ `False | `True | `Marks of marks ]

  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  val fold :
    ?force:'a force ->
    ?uniq:uniq ->
    ?pre:'a node_fn ->
    ?post:'a node_fn ->
    (key -> contents -> 'a -> 'a Lwt.t) ->
    tree ->
    'a ->
    'a Lwt.t

  type stats = {
    nodes : int;
    leafs : int;
    skips : int;
    depth : int;
    width : int;
  }

  val pp_stats : stats Fmt.t

  val stats : ?force:bool -> tree -> stats Lwt.t

  type concrete =
    [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

  val of_concrete : concrete -> tree

  val to_concrete : tree -> concrete Lwt.t

  val clear : ?depth:int -> tree -> unit

  type counters = {
    mutable contents_hash : int;
    mutable contents_find : int;
    mutable contents_add : int;
    mutable node_hash : int;
    mutable node_mem : int;
    mutable node_add : int;
    mutable node_find : int;
    mutable node_val_v : int;
    mutable node_val_find : int;
    mutable node_val_list : int;
  }

  val counters : unit -> counters

  val dump_counters : unit Fmt.t

  val reset_counters : unit -> unit

  val inspect : tree -> [ `Contents | `Node of [ `Map | `Hash | `Value ] ]
end

module type STORE = sig
  type repo

  type t

  type step

  type key

  type metadata

  type contents

  type node

  type tree = [ `Node of node | `Contents of contents * metadata ]

  type hash

  type commit

  type branch

  type slice

  type lca_error = [ `Max_depth_reached | `Too_many_lcas ]

  type ff_error = [ `No_change | `Rejected | lca_error ]

  module Repo : sig
    type t = repo

    val v : config -> t Lwt.t

    val close : t -> unit Lwt.t

    val heads : t -> commit list Lwt.t

    val branches : t -> branch list Lwt.t

    val export :
      ?full:bool ->
      ?depth:int ->
      ?min:commit list ->
      ?max:commit list ->
      t ->
      slice Lwt.t

    val import : t -> slice -> (unit, [ `Msg of string ]) result Lwt.t
  end

  val empty : Repo.t -> t Lwt.t

  val master : Repo.t -> t Lwt.t

  val of_branch : Repo.t -> branch -> t Lwt.t

  val of_commit : commit -> t Lwt.t

  val repo : t -> Repo.t

  val tree : t -> tree Lwt.t

  module Status : sig
    type t = [ `Empty | `Branch of branch | `Commit of commit ]

    val t : Repo.t -> t Type.t

    val pp : t Fmt.t
  end

  val status : t -> Status.t

  module Head : sig
    val list : Repo.t -> commit list Lwt.t

    val find : t -> commit option Lwt.t

    val get : t -> commit Lwt.t

    val set : t -> commit -> unit Lwt.t

    val fast_forward :
      t -> ?max_depth:int -> ?n:int -> commit -> (unit, ff_error) result Lwt.t

    val test_and_set :
      t -> test:commit option -> set:commit option -> bool Lwt.t

    val merge :
      into:t ->
      info:Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Merge.conflict) result Lwt.t
  end

  module Hash : HASH with type t = hash

  module Commit : sig
    type t = commit

    val t : Repo.t -> t Type.t

    val pp_hash : t Fmt.t

    val v : Repo.t -> info:Info.t -> parents:hash list -> tree -> commit Lwt.t

    val tree : commit -> tree

    val parents : commit -> hash list

    val info : commit -> Info.t

    val hash : commit -> hash

    val of_hash : Repo.t -> hash -> commit option Lwt.t
  end

  module Contents : sig
    include CONTENTS with type t = contents

    val hash : contents -> hash

    val of_hash : Repo.t -> hash -> contents option Lwt.t
  end

  module Tree : sig
    include
      TREE
        with type step := step
         and type key := key
         and type metadata := metadata
         and type contents := contents
         and type node := node
         and type tree := tree

    val hash : tree -> hash

    val of_hash : Repo.t -> hash -> tree option Lwt.t

    val shallow : Repo.t -> hash -> tree
  end

  val kind : t -> key -> [ `Contents | `Node ] option Lwt.t

  val list : t -> key -> (step * [ `Contents | `Node ]) list Lwt.t

  val mem : t -> key -> bool Lwt.t

  val mem_tree : t -> key -> bool Lwt.t

  val find_all : t -> key -> (contents * metadata) option Lwt.t

  val find : t -> key -> contents option Lwt.t

  val get_all : t -> key -> (contents * metadata) Lwt.t

  val get : t -> key -> contents Lwt.t

  val find_tree : t -> key -> tree option Lwt.t

  val get_tree : t -> key -> tree Lwt.t

  val hash : t -> key -> hash option Lwt.t

  type write_error =
    [ Merge.conflict | `Too_many_retries of int | `Test_was of tree option ]

  val set :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    contents ->
    (unit, write_error) result Lwt.t

  val set_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    contents ->
    unit Lwt.t

  val set_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    (unit, write_error) result Lwt.t

  val set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    unit Lwt.t

  val remove :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    (unit, write_error) result Lwt.t

  val remove_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    unit Lwt.t

  val test_and_set :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:contents option ->
    set:contents option ->
    (unit, write_error) result Lwt.t

  val test_and_set_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:contents option ->
    set:contents option ->
    unit Lwt.t

  val test_and_set_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:tree option ->
    set:tree option ->
    (unit, write_error) result Lwt.t

  val test_and_set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:tree option ->
    set:tree option ->
    unit Lwt.t

  val merge :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    key ->
    contents option ->
    (unit, write_error) result Lwt.t

  val merge_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    key ->
    contents option ->
    unit Lwt.t

  val merge_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    key ->
    tree option ->
    (unit, write_error) result Lwt.t

  val merge_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    key ->
    tree option ->
    unit Lwt.t

  val with_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option Lwt.t) ->
    (unit, write_error) result Lwt.t

  val with_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option Lwt.t) ->
    unit Lwt.t

  val clone : src:t -> dst:branch -> t Lwt.t

  type watch

  val watch : t -> ?init:commit -> (commit diff -> unit Lwt.t) -> watch Lwt.t

  val watch_key :
    t ->
    key ->
    ?init:commit ->
    ((commit * tree) diff -> unit Lwt.t) ->
    watch Lwt.t

  val unwatch : watch -> unit Lwt.t

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result Lwt.t

  val merge_into : into:t -> t merge

  val merge_with_branch : t -> branch merge

  val merge_with_commit : t -> commit merge

  val lcas :
    ?max_depth:int -> ?n:int -> t -> t -> (commit list, lca_error) result Lwt.t

  val lcas_with_branch :
    t ->
    ?max_depth:int ->
    ?n:int ->
    branch ->
    (commit list, lca_error) result Lwt.t

  val lcas_with_commit :
    t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    (commit list, lca_error) result Lwt.t

  module History : Graph.Sig.P with type V.t = commit

  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t Lwt.t

  val last_modified : ?depth:int -> ?n:int -> t -> key -> commit list Lwt.t

  module Branch : sig
    val mem : Repo.t -> branch -> bool Lwt.t

    val find : Repo.t -> branch -> commit option Lwt.t

    val get : Repo.t -> branch -> commit Lwt.t

    val set : Repo.t -> branch -> commit -> unit Lwt.t

    val remove : Repo.t -> branch -> unit Lwt.t

    val list : Repo.t -> branch list Lwt.t

    val watch :
      Repo.t ->
      branch ->
      ?init:commit ->
      (commit diff -> unit Lwt.t) ->
      watch Lwt.t

    val watch_all :
      Repo.t ->
      ?init:(branch * commit) list ->
      (branch -> commit diff -> unit Lwt.t) ->
      watch Lwt.t

    include BRANCH with type t = branch
  end

  module Key : PATH with type t = key and type step = step

  module Metadata : METADATA with type t = metadata

  val step_t : step Type.t

  val key_t : key Type.t

  val metadata_t : metadata Type.t

  val contents_t : contents Type.t

  val node_t : node Type.t

  val tree_t : tree Type.t

  val commit_t : Repo.t -> commit Type.t

  val branch_t : branch Type.t

  val slice_t : slice Type.t

  val kind_t : [ `Contents | `Node ] Type.t

  val lca_error_t : lca_error Type.t

  val ff_error_t : ff_error Type.t

  val write_error_t : write_error Type.t

  module Private : sig
    include
      PRIVATE
        with type Contents.value = contents
         and module Hash = Hash
         and module Node.Path = Key
         and type Node.Metadata.t = metadata
         and type Branch.key = branch
         and type Slice.t = slice
         and type Repo.t = repo
  end

  type remote += E of Private.Sync.endpoint

  val to_private_node : node -> Private.Node.value option Lwt.t

  val of_private_node : repo -> Private.Node.value -> node

  val to_private_commit : commit -> Private.Commit.value

  val of_private_commit : repo -> Private.Commit.value -> commit

  val save_contents : [> `Write ] Private.Contents.t -> contents -> hash Lwt.t

  val save_tree :
    ?clear:bool ->
    repo ->
    [> `Write ] Private.Contents.t ->
    [ `Read | `Write ] Private.Node.t ->
    tree ->
    hash Lwt.t
end

module type MAKER = functor
  (M : METADATA)
  (C : CONTENTS)
  (P : PATH)
  (B : BRANCH)
  (H : HASH)
  ->
  STORE
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t

type remote += Store : (module STORE with type t = 'a) * 'a -> remote

module type SYNC_STORE = sig
  (** {1 Native Synchronization} *)

  type db
  (** Type type for store handles. *)

  type commit
  (** The type for store heads. *)

  type status = [ `Empty | `Head of commit ]
  (** The type for remote status. *)

  val status_t : db -> status Type.t
  (** [status_t db] is the value type for {!status} of remote [db]. *)

  val pp_status : status Fmt.t
  (** [pp_status] pretty-prints return statuses. *)

  val fetch :
    db -> ?depth:int -> remote -> (status, [ `Msg of string ]) result Lwt.t
  (** [fetch t ?depth r] populate the local store [t] with objects for
      the remote store [r], using [t]'s current branch. The [depth]
      parameter limits the history depth. Return [`Empty] if either the
      local or remote store do not have a valid head. *)

  val fetch_exn : db -> ?depth:int -> remote -> status Lwt.t
  (** Same as {!fetch} but raise [Invalid_argument] if either the
      local or remote store do not have a valid head. *)

  type pull_error = [ `Msg of string | Merge.conflict ]
  (** The type for pull errors. *)

  val pp_pull_error : pull_error Fmt.t
  (** [pp_push_error] pretty-prints pull errors. *)

  val pull :
    db ->
    ?depth:int ->
    remote ->
    [ `Merge of Info.f | `Set ] ->
    (status, pull_error) result Lwt.t
  (** [pull t ?depth r s] is similar to {{!Sync.fetch}fetch} but it
      also updates [t]'s current branch. [s] is the update strategy:

      {ul
      {- [`Merge] uses [Head.merge]. Can return a conflict.}
      {- [`Set] uses [S.Head.set].}
      } *)

  val pull_exn :
    db -> ?depth:int -> remote -> [ `Merge of Info.f | `Set ] -> status Lwt.t
  (** Same as {!pull} but raise [Invalid_arg] in case of conflict. *)

  type push_error = [ `Msg of string | `Detached_head ]
  (** The type for push errors. *)

  val pp_push_error : push_error Fmt.t
  (** [pp_push_error] pretty-prints push errors. *)

  val push : db -> ?depth:int -> remote -> (status, push_error) result Lwt.t
  (** [push t ?depth r] populates the remote store [r] with objects
      from the current store [t], using [t]'s current branch. If [b]
      is [t]'s current branch, [push] also updates the head of [b] in
      [r] to be the same as in [t].

      {b Note:} {e Git} semantics is to update [b] only if the new
      head if more recent. This is not the case in {e Irmin}. *)

  val push_exn : db -> ?depth:int -> remote -> status Lwt.t
  (** Same as {!push} but raise [Invalid_argument] if an error
      happens. *)
end
