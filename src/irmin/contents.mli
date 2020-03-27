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

(** Values. *)

type json =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `O of (string * json) list
  | `A of json list ]

module String : S.CONTENTS with type t = string

module Json : S.CONTENTS with type t = (string * json) list

module Json_value : S.CONTENTS with type t = json

module Json_tree (Store : Store.S with type contents = json) : sig
  include S.CONTENTS with type t = json

  val to_concrete_tree : t -> Store.Tree.concrete

  val of_concrete_tree : Store.Tree.concrete -> t

  val get_tree : Store.tree -> Store.key -> json Lwt.t

  val set_tree : Store.tree -> Store.key -> json -> Store.tree Lwt.t

  val get : Store.t -> Store.key -> json Lwt.t

  val set : Store.t -> Store.key -> json -> info:Info.f -> unit Lwt.t
end

module V1 : sig
  module String : S.CONTENTS with type t = string
end

(** [CONTENT_ADDRESSABLE_STORE_EXT] is {!S.CONTENT_ADDRESSABLE_STORE} with the
    requirements that the keys are {!S.HASH}es and the values are {!S.CONTENTS}. *)
module type CONTENT_ADDRESSABLE_STORE_EXT = sig
  include S.CONTENT_ADDRESSABLE_STORE

  module Key : S.HASH with type t = key

  module Val : S.CONTENTS with type t = value
end

module type MAKER = functor (C : CONTENT_ADDRESSABLE_STORE_EXT) ->
  S.CONTENTS_STORE
    with type 'a t = 'a C.t
     and type key = C.key
     and type value = C.value

module Store : MAKER

(** [TYPED_CONTENT_ADDRESSABLE_STORE_EXT] is
    {!S.TYPED_CONTENT_ADDRESSABLE_STORE} with the requirements that keys have a
    {!S.HASH} representation and the values are sub-components of a root type
    described by {!S.TYPED_CONTENTS}. *)
module type TYPED_CONTENT_ADDRESSABLE_STORE_EXT = sig
  include S.TYPED_CONTENT_ADDRESSABLE_STORE

  module Key :
    S.POLY_KEY with type 'value t = 'value key and type 'value typ = 'value typ

  module Root : S.TYPED_CONTENTS with type ('a, _) Shape.t = 'a Key.typ
end

module type TYPED_MAKER = functor (C : TYPED_CONTENT_ADDRESSABLE_STORE_EXT) ->
  S.TYPED_CONTENTS_STORE

module Typed_store : TYPED_MAKER
