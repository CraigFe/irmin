(* When a type is annotated with the [nobuiltin] annotation, it should be
   considered as an abstract type (i.e. don't pull representations from
   [Irmin.Type]). *)

type unit = string [@@deriving irmin]

type t = (unit[@nobuiltin]) [@@deriving irmin]

type foo = (unit[@irmin.nobuiltin]) [@@deriving irmin]

(* TODO: uncomment once https://github.com/mirage/irmin/pull/970 is merged *)

(* let (_ : unit_t Irmin.Type.t) = t
 * 
 * let (_ : unit_t Irmin.Type.t) = foo_t *)
