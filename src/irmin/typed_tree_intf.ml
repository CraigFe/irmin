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

module type PICKLER = sig
  type 'a t

  module Pickled : Type.S

  val pickle : 'value t -> 'value -> Pickled.t

  val unpickle : 'value t -> Pickled.t -> 'value option
end

module type PATH = sig
  type 'a assoc

  type 'a tree

  type ('s, 'a) t
  (** The type of 'paths' from a store value of type ['s] to a store value of
      type ['a]. *)

  val pp : ('s, 'a) t Fmt.t
  (** The pretty-printer for paths. *)

  val ( / ) : ('s, 'inner) t -> ('inner, 'a) t -> ('s, 'a) t
  (** Left-to-right composition of paths. *)

  val empty : ('s, 's) t
  (** The empty path. [empty] is the identity of {{!( / )} path composition}. *)

  (* val find : string -> ('a assoc, 'a) t *)
  (** [find s] is the path that indexes an association list with the key [s]. *)

  (* val steps : string list -> ('a tree, 'a) t *)
  (** [steps \[s_1; ...; s_n\]] is the path from the root of a tree to a leaf,
      following children with indices \[s_1; ...; s_n\]. *)

  (** Convenient syntax for a list of paths of differing type. *)
  type 'head hlist =
    | [] : unit hlist
    | ( :: ) : ('elt * 'head hlist) -> ('elt * 'head) hlist
end

module type S = sig
  type step

  type ('a, 'b) t
  (** The type of {i representations of} the types of Irmin stores. Values of
      this type can be used to build Irmin stores containing more than one
      content type. *)

  val merge : ('a, _) t -> 'a Merge.t
  (** Shapes imply a merge operator. *)

  val type_ : ('a, _) t -> 'a Type.t
  (** Shapes imply a serialisation format. *)

  type empty = |

  type 'a addr
  (** Indirection via a store *)

  val addr : ('a, 'b) t -> ('a addr, 'b) t

  val addr_t : 'a Type.t -> 'a addr Type.t

  type 'a assoc
  (** An [s assoc] is the type of a map from strings to stores of type [s t]. *)

  type 'a assoc_concrete = (step * 'a) array

  val assoc : ('a, 'b) t -> ('a assoc, 'b assoc_concrete) t
  (** [assoc t] represents the type of association lists between strings and the
      given store type [t]. Stores with such types can be indexed with the
      {!Path.find} constructor. *)

  type 'a tree
  (** An [s tree] is the type of a tree with leaves of type [s t] and branches
      with string-indexed children. Roughly equivalent to:

      {[ type 'a tree = Branch of (string * 'a tree) array | Leaf of 'a t ]} *)

  type 'a tree_concrete =
    | Branch of 'a tree_concrete assoc_concrete
    | Leaf of 'a

  val tree_concrete_t : 'a Type.t -> 'a tree_concrete Type.t

  val tree : ('a, 'b) t -> ('a tree, 'b tree_concrete) t
  (** [tree l] represents the type of trees with leaves of the given store type
      [l]. Stores with such types can be indexed with the {!Path.steps}
      constructor. *)

  val primitive : 'a Type.t -> 'a Merge.t -> ('a, 'a) t
  (** [primitive t m] represents the type of stores containing a single value of
      type [t] with merge operation [m]. Stores with such types can be indexed
      with the {!Path.id} path. *)

  module Path : PATH with type 'a assoc := 'a assoc and type 'a tree := 'a tree

  (** {1 Record types} *)

  type ( 'record,
         'constructor,
         'remaining_fields,
         'paths,
         'paths_nil )
       open_record

  val record :
    string ->
    'constructor ->
    ('record, 'constructor, 'constructor, 'paths_nil, 'paths_nil) open_record

  type ('record, 'component) field

  val field :
    step ->
    ('component, _) t ->
    ('record -> 'component -> 'record) ->
    ('record -> 'component) ->
    ('record, 'component) field

  val ( |+ ) :
    ( 'record,
      'constructor,
      'component -> 'remaining,
      'paths,
      ('record, 'component) Path.t * 'paths_nil )
    open_record ->
    ('record, 'component) field ->
    ('record, 'constructor, 'remaining, 'paths, 'paths_nil) open_record

  val sealr :
    ('record, 'constructor, 'record, 'paths, unit) open_record ->
    'record Type.t ->
    ('record, empty) t * 'paths Path.hlist

  (** {1 Variant types} *)

  type ( 'variant,
         'pattern,
         'remaining,
         'remaining_nil,
         'paths,
         'paths_nil )
       open_variant
  (** The type of 'open' representations of a variant type ['variant] with
      pattern matchings of type ['pattern].

      - ['remaining_cases] represents the remaining constructors to be described
        using the {!(|~)} operator. An open variant initially satisfies
        ['remaining_cases = 'pattern] and can be {{!sealv} sealed} once
        ['remaining_cases] = ['variant].

      - ['paths] gives the types of {!Path.t}'s corresponding to the cases
        passed so far. When the representation is {{!sealv} sealed},
        ['paths_nil] is set to [unit] and the paths can be extracted using the
        {!Path.hlist} syntax *)

  val variant :
    string ->
    'pattern ->
    ( 'variant,
      'pattern,
      'pattern,
      'pattern,
      'paths_nil,
      'paths_nil )
    open_variant
  (** Construct a new open representation of a variant with a given name and
      pattern matcher. To complete the representation, add cases with {!(|~)}
      and then seal the variant with {!sealv}. *)

  type ('variant, 'arg, 'constructor) case

  type 'a case_p

  val case0 : step -> 'variant -> ('variant, unit, unit -> 'variant case_p) case
  (** [case n v] is a representation of a variant constructor [v] with no
      arguments and name [n]. *)

  val case1 :
    step ->
    ('arg, _) t ->
    ('arg -> 'variant) ->
    ('variant, 'arg, 'arg -> 'variant case_p) case

  val ( |~ ) :
    ( 'variant,
      'pattern,
      'remaining_cases,
      'constructor -> 'remaining_nil,
      'paths,
      ('variant, 'arg) Path.t * 'paths_nil )
    open_variant ->
    ('variant, 'arg, 'constructor) case ->
    ( 'variant,
      'pattern,
      'remaining_cases,
      'remaining_nil,
      'paths,
      'paths_nil )
    open_variant

  val sealv :
    ( 'variant,
      'pat,
      'pat,
      'variant -> 'variant case_p,
      'paths,
      unit )
    open_variant ->
    'variant Type.t ->
    ('variant, empty) t * 'paths Path.hlist
end

module type MAKER = functor (Step : Type.S) (_ : Type.S) (Addr : Type.S2) ->
  S with type step = Step.t

module type Typed_tree = sig
  module type PATH = PATH

  module type S = S

  module type MAKER = MAKER

  module Make : MAKER
end
