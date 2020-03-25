(*
 * Copyright (c) 2020 Craig Ferguson <craig@tarides.com>
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

type 'a t
(** The type of {i representations of} the types of Irmin stores.
    Values of this type can be used to build Irmin stores containing more than
    one content type. *)

type 'a assoc
(** An [s assoc] is the type of a map from strings to stores of type [s t]. *)

val assoc : 'a t -> 'a assoc t
(** [assoc t] represents the type of association lists between strings and the
    given store type [t]. Stores with such types can be indexed with the
    {!Path.find} constructor. *)

type 'a tree
(** An [s tree] is the type of a tree with leaves of type [s t] and branches
    with string-indexed children. Roughly equivalent to:

    {[ type 'a tree = Branch of (string * 'a tree) array | Leaf of 'a t ]} *)

val tree : 'a t -> 'a tree t
(** [tree l] represents the type of trees with leaves of the given store type
    [l]. Stores with such types can be indexed with the {!Path.steps}
    constructor. *)

val primitive : 'a Type.t -> 'a t
(** [primitive t] represents the type of stores containing a single value of
    type [t]. Stores with such types can be indexed with the {!Path.id} path. *)

module Path : sig
  type ('s, 'a) t
  (** The type of 'paths' from a store value of type ['s] to a store value of
      type ['a]. *)

  val ( / ) : ('s, 'inner) t -> ('inner, 'a) t -> ('s, 'a) t
  (** Left-to-right composition of paths. *)

  val id : ('s, 's) t
  (** The identity path. *)

  val find : string -> ('a assoc, 'a) t
  (** [find s] is the path that indexes an association list with the key [s]. *)

  val steps : string list -> ('a tree, 'a) t
  (** [steps \[s_1; ...; s_n\]] is the path from the root of a tree to a leaf,
      following children with indices \[s_1; ...; s_n\]. *)

  (** Convenient syntax for a list of paths of differing type. *)
  type ('head, 'nil) hlist =
    | [] : ('nil, 'nil) hlist
    | ( :: ) : ('elt * ('head, 'nil) hlist) -> ('elt * 'head, 'nil) hlist
end

(** {1 Record types} *)

type ('record, 'constructor, 'remaining_fields, 'paths, 'paths_nil) open_record

val record :
  string ->
  'constructor ->
  ('record, 'constructor, 'constructor, 'paths_nil, 'paths_nil) open_record

type ('record, 'component) field

val field :
  string ->
  'component t ->
  ('record -> 'component -> 'record) ->
  ('record -> 'component) ->
  ('record, 'component) field

(** {1 Variant types} *)

type ('variant, 'pattern, 'remaining, 'remaining_nil, 'paths, 'paths_nil) open_variant
(** The type of 'open' representations of a variant type ['variant] with pattern
    matchings of type ['pattern].

    - ['remaining_cases] represents the remaining constructors to be described
      using the {!(|~)} operator. An open variant initially satisfies
      ['remaining_cases = 'pattern] and can be {{!sealv} sealed} once
      ['remaining_cases] = ['variant].

    - ['paths] gives the types of {!Path.t}'s corresponding to the cases passed
      so far. When the representation is {{!sealv} sealed}, ['paths_nil] is set
      to [unit] and the paths can be extracted using the {!Path.hlist} syntax *)

val variant :
  string ->
  'pattern ->
  ('variant, 'pattern, 'remaining_nil, 'remaning_nil, 'paths_nil, 'paths_nil) open_variant
(** Construct a new open representation of a variant with a given name and
    pattern matcher. To complete the representation, add cases with {!(|~)} and
    then seal the variant with {!sealv}. *)

type ('variant, 'arg, 'constructor) case

val case0 : string -> 'variant -> ('variant, unit) case
(** [case n v] is a representation of a variant constructor [v] with no
    arguments and name [n]. *)

val case1 : string -> 'arg t -> ('arg -> 'variant) -> ('variant, 'arg) case

val ( |~ ) :
  ( 'variant,
    'pattern,
    'remaining_cases,
    'constructor -> 'remaining_nil,
    'paths,
    ('variant, 'arg) Path.t * 'paths_nil )
  open_variant ->
  ('variant, 'arg, 'constructor) case ->
  ('variant, 'pattern, 'remaining_cases, 'rem_nil, 'paths, 'paths_nil) open_variant
