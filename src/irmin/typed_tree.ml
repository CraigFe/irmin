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

open Typed_tree_intf

module type PATH = PATH

module type MAKER = MAKER

module type S = S

module Witness = Irmin_type.Witness

module Make (Step : Type.S) (Hash : Type.S) (Addr : Type.S2) = struct
  type step = Step.t

  type empty = |

  (* Wrap [Addr.t] to have an injective parameter *)
  type 'a addr = Addr of 'a Addr.t | Void of 'a * empty
  [@@ocaml.warning "-37"]

  let addr_t : type a. a Type.t -> a addr Type.t =
   fun elt_t ->
    Type.map (Addr.t elt_t)
      (fun a -> Addr a)
      (function Addr a -> a | Void _ -> .)

  type 'a assoc = (step * 'a addr) array

  type 'a assoc_concrete = (step * 'a) array

  type 'a tree = Branch_node of 'a tree assoc | Leaf_node of 'a

  type 'a tree_concrete =
    | Branch of 'a tree_concrete assoc_concrete
    | Leaf of 'a

  (* Types that shouldn't go in the exported namespace *)
  module Types = struct
    type (_, _) typ =
      | Typ_record : 'a record -> ('a, empty) typ
      | Typ_variant : 'a variant -> ('a, empty) typ
      | Typ_addr : ('a, 'b) typ -> ('a addr, 'b) typ
      | Typ_assoc : ('a, 'b) typ -> ('a assoc, 'b assoc_concrete) typ
      | Typ_tree : ('a, 'b) typ -> ('a tree, 'b tree_concrete) typ
      | Typ_primitive : {
          t_generic : 'a Type.t;
          t_merge : 'a Merge.t;
        }
          -> ('a, 'a) typ

    (* Records *)
    and 'record record =
      | Record : {
          r_name : string;
          r_type : 'record Type.t;
          r_witness : 'record Witness.t;
          r_fields : ('record, 'cons, _, _) fields;
          r_constructor : 'cons;
        }
          -> 'record record

    and ('record, 'field) field =
      | Field : {
          f_name : step;
          f_type : ('field, 'field_concrete) typ;
          f_get : 'record -> 'field;
          f_update : 'record -> 'field -> 'record;
        }
          -> ('record, 'field) field

    and (_, _, _, _) fields =
      | Fields_nil : ('record, 'constr, 'paths_nil, 'paths_nil) fields
      | Fields_cons :
          ('record, 'field) field
          * ('record, 'constr, 'paths, 'paths_nil) fields
          -> ( 'record,
               'field -> 'constr,
               ('record, 'field) path * 'paths,
               'paths_nil )
             fields

    (* Variants *)
    and 'variant variant = {
      v_witness : 'variant Witness.t;
      v_name : string;
      v_cases : 'variant case_exists array;
      v_get : 'variant -> 'variant case_typeable;
      v_type : 'variant Type.t;
    }

    and ('variant, 'arg, 'constr) case =
      | Case : {
          c_name : step;
          c_tag : int;
          c_type : ('arg, 'arg_concrete) typ;
          c_constructor : 'arg -> 'variant;
          c_witness : 'arg Witness.t;
        }
          -> ('variant, 'arg, 'arg -> 'variant case_typeable) case

    and 'variant case_exists =
      | Case_exists : ('variant, 'arg, 'constr) case -> 'variant case_exists

    and 'variant case_typeable =
      | Case_typeable :
          ('variant, 'arg, 'constr) case * 'arg
          -> 'variant case_typeable

    and ('variant, 'pat, 'pat_nil, 'paths, 'paths_nil) cases =
      | Cases_nil : ('variant, 'pat_nil, 'pat_nil, 'paths_nil, 'paths_nil) cases
      | Cases_cons :
          ('variant, 'case, 'constr) case
          * ('variant, 'remaining, 'pat_nil, 'paths, 'paths_nil) cases
          -> ( 'variant,
               'constr -> 'remaining,
               'pat_nil,
               ('variant, 'case) path * 'paths,
               'paths_nil )
             cases

    and 'a boxed_typ = Boxed_type : ('a, 'a_conc) typ -> 'a boxed_typ

    and (_, _) path =
      | Path : {
          p_project_type : 's boxed_typ -> 'a boxed_typ;
          p_steps : step list;
          p_get : 's -> 'a option Lwt.t;
          p_update : 's -> 'a -> 's Lwt.t;
        }
          -> ('s, 'a) path
  end

  open Types

  type ('a, 'b) t = ('a, 'b) typ

  let addr typ = Typ_addr typ

  let assoc typ = Typ_assoc typ

  let tree typ = Typ_tree typ

  let assoc_concrete_t elt_t = Type.(array (pair Step.t elt_t))

  let merge : type a b. (a, b) typ -> a Merge.t = function
    | Typ_primitive { t_merge; _ } -> t_merge
    | _ -> (* TODO *) assert false

  let assoc_t : type a. a Type.t -> a assoc Type.t =
   fun elt_t -> Type.(array (pair Step.t (addr_t elt_t)))

  let tree_t elt_t =
    let open Type in
    mu (fun tree_t ->
        variant "tree" (fun branch leaf ->
          function Branch_node b -> branch b | Leaf_node l -> leaf l)
        |~ case1 "branch_node" (assoc_t tree_t) (fun b -> Branch_node b)
        |~ case1 "leaf_node" elt_t (fun l -> Leaf_node l)
        |> sealv)

  let tree_concrete_t elt_t =
    let open Type in
    mu (fun tree_concrete_t ->
        variant "tree_concrete" (fun branch leaf ->
          function Branch b -> branch b | Leaf l -> leaf l)
        |~ case1 "branch" (assoc_concrete_t tree_concrete_t) (fun b -> Branch b)
        |~ case1 "leaf" elt_t (fun l -> Leaf l)
        |> sealv)

  let primitive t_generic t_merge = Typ_primitive { t_generic; t_merge }

  let rec type_ : type a b. (a, b) typ -> a Type.t = function
    | Typ_primitive { t_generic; _ } -> t_generic
    | Typ_tree t -> tree_t (type_ t)
    | Typ_assoc t -> assoc_t (type_ t)
    | Typ_record (Record { r_type; _ }) -> r_type
    | Typ_variant { v_type; _ } -> v_type
    | Typ_addr t -> addr_t (type_ t)

  module Path = struct
    type ('a, 'b) t = ('a, 'b) path

    let pp : type s a. (s, a) t Fmt.t =
     fun ppf (Path { p_steps; _ }) ->
      let pp_step = Type.pp Step.t in
      Fmt.pf ppf "%a" Fmt.Dump.(list pp_step) p_steps

    let ( / ) (Path outer) (Path inner) =
      let open Lwt.Infix in
      let p_steps = outer.p_steps @ inner.p_steps in
      let p_project_type s_typ =
        let mid_typ = outer.p_project_type s_typ in
        inner.p_project_type mid_typ
      in
      let p_get s =
        outer.p_get s >>= function
        | Some mid -> inner.p_get mid
        | None -> Lwt.return None
      in
      let p_update s a =
        outer.p_get s >>= function
        | None -> Lwt.return s
        | Some mid -> inner.p_update mid a >>= fun mid -> outer.p_update s mid
      in
      Path { p_steps; p_project_type; p_get; p_update }

    let empty =
      let p_get s = Lwt.return (Some s) in
      let p_update _ s = Lwt.return s in
      Path { p_steps = []; p_project_type = Fun.id; p_get; p_update }

    type 'head hlist =
      | [] : unit hlist
      | ( :: ) : ('elt * 'head hlist) -> ('elt * 'head) hlist
  end

  (** Generic combinators for product types **)
  module Record = struct
    module Unwitnessed = struct
      type ('record, 'cons, 'paths, 'paths_nil) t = {
        name : string;
        cons : 'cons;
        fields : ('record, 'cons, 'paths, 'paths_nil) fields;
      }
    end

    type ('record, 'constr, 'remaining, 'paths, 'paths_nil) open_record = {
      open_record :
        'hole. ('record, 'remaining, 'paths_nil, 'hole) fields ->
        (* Append the two path lists at the type level *)
        ('record, 'constr, 'paths, 'hole) Unwitnessed.t;
    }
    [@@unboxed]

    let field f_name f_type f_update f_get =
      Field { f_name; f_type; f_get; f_update }

    let record :
        type r. string -> r -> ('a, r, r, 'paths_nil, 'paths_nil) open_record =
     fun name cons ->
      let open_record fields = Unwitnessed.{ name; cons; fields } in
      { open_record }

    let ( |+ ) :
        type r c ft rem paths paths_nil.
        (r, c, ft -> rem, paths, (r, ft) path * paths_nil) open_record ->
        (r, ft) field ->
        (r, c, rem, paths, paths_nil) open_record =
     fun { open_record = previous } field ->
      let open_record' :
          type hole.
          (r, rem, paths_nil, hole) fields -> (r, c, paths, hole) Unwitnessed.t
          =
       fun fs -> previous (Fields_cons (field, fs))
      in
      { open_record = open_record' }

    (* Ground path list with [unit] *)
    let sealr :
        type record_t cons lens.
        (record_t, cons, record_t, lens, unit) open_record ->
        record_t Type.t ->
        (record_t, empty) typ * lens Path.hlist =
     fun { open_record = r } r_type ->
      let Unwitnessed.{ name; cons; fields } = r Fields_nil in
      let r_witness = Witness.make () in
      let paths =
        let rec inner : type a l. (record_t, a, l, unit) fields -> l Path.hlist
            = function
          | Fields_nil -> []
          | Fields_cons (Field { f_get; f_update; f_type; f_name }, fs) ->
              let p_steps = [ f_name ] in
              let p_project_type _ = Boxed_type f_type in
              let p_get s = Lwt.return (Some (f_get s)) in
              let p_update s a = Lwt.return (f_update s a) in
              let path = Path { p_steps; p_project_type; p_get; p_update } in
              path :: inner fs
        in
        inner fields
      in
      ( Typ_record
          (Record
             {
               r_witness;
               r_name = name;
               r_type;
               r_fields = fields;
               r_constructor = cons;
             }),
        paths )
  end

  (** Generic combinators for sum types *)
  module Variant = struct
    type ('v, 'pat, 'rem, 'rem_nil, 'prism, 'prism_nil) open_variant = {
      open_variant :
        'hole1 'hole2. ('v, 'rem_nil, 'hole1, 'prism_nil, 'hole2) cases ->
        string * 'hole1 * ('v, 'rem, 'hole1, 'prism, 'hole2) cases;
      next_tag : int;
    }

    let rec add_cases_to_destructor :
        type v pat pat_nil opt opt_nil.
        pat -> (v, pat, pat_nil, opt, opt_nil) cases -> pat_nil =
     fun p -> function
      | Cases_nil -> p
      | Cases_cons (Case case, cs) ->
          add_cases_to_destructor (p (fun v -> Case_typeable (Case case, v))) cs

    let variant :
        type v p opt. string -> p -> (v, p, p, p, opt, opt) open_variant =
     fun n p ->
      let open_variant cs = (n, add_cases_to_destructor p cs, cs) in
      { open_variant; next_tag = 0 }

    (** Cases are accumulated while their tags remain unknown. Tags are assigned
        when the variant is sealed. *)
    type ('v, 'case, 'constr) untagged_case =
      | Untagged of (int -> ('v, 'case, 'constr) case)
    [@@unboxed]

    let ( |~ ) :
        type v c constr pat rem rem_nil opt opt_nil.
        ( v,
          pat,
          rem,
          constr -> rem_nil,
          opt,
          (v, c) path * opt_nil )
        open_variant ->
        (v, c, constr) untagged_case ->
        (v, pat, rem, rem_nil, opt, opt_nil) open_variant =
     fun { open_variant = previous; next_tag } (Untagged case) ->
      let open_variant' cs = previous (Cases_cons (case next_tag, cs)) in
      { open_variant = open_variant'; next_tag = next_tag + 1 }

    let array_of_case_list cases =
      let rec inner :
          type variant pat pat_nil pri pri_nil.
          (variant, pat, pat_nil, pri, pri_nil) cases ->
          variant case_exists list = function
        | Cases_nil -> []
        | Cases_cons (c, cs) -> Case_exists c :: inner cs
      in
      inner cases |> Array.of_list

    let preview :
        type v c. (v -> v case_typeable) -> c Witness.t -> int -> v -> c option
        =
     fun vget type_expected tag_expected v ->
      let (Case_typeable
            (Case { c_tag = tag_actual; c_witness = type_actual; _ }, elt)) =
        vget v
      in
      if tag_actual = tag_expected then
        Witness.cast type_actual type_expected elt
      else None

    let paths_of_cases (type variant) (v_get : variant -> variant case_typeable)
        =
      let rec inner :
          type a b paths. (variant, a, b, paths, unit) cases -> paths Path.hlist
          = function
        | Cases_nil -> []
        | Cases_cons
            ( Case
                {
                  c_constructor;
                  c_tag = tag_expected;
                  c_witness = type_expected;
                  c_type;
                  c_name;
                },
              cs ) ->
            let p_steps = [ c_name ] in
            let p_project_type _ = Boxed_type c_type in
            let p_update s a =
              let (Case_typeable (Case { c_tag = tag_actual; _ }, _)) =
                v_get s
              in
              (if Int.equal tag_actual tag_expected then c_constructor a else s)
              |> Lwt.return
            in
            let p_get s =
              preview v_get type_expected tag_expected s |> Lwt.return
            in
            let path = Path { p_steps; p_project_type; p_get; p_update } in
            Path.(path :: inner cs)
      in
      inner

    let sealv :
        type variant pat prisms.
        ( variant,
          pat,
          pat,
          variant -> variant case_typeable,
          prisms,
          unit )
        open_variant ->
        variant Type.t ->
        (variant, empty) typ * prisms Path.hlist =
     fun { open_variant = v; _ } v_type ->
      let v_name, v_get, cases = v Cases_nil in
      let v_witness = Witness.make () in
      let v_cases = array_of_case_list cases in
      let paths = paths_of_cases v_get cases in
      (Typ_variant { v_witness; v_name; v_cases; v_get; v_type }, paths)

    let case1 c_name c_type c_constructor =
      Untagged
        (fun c_tag ->
          Case
            {
              c_tag;
              c_name;
              c_type;
              c_constructor;
              c_witness = Witness.make ();
            })

    let case0 c_name c_singleton =
      Untagged
        (fun c_tag ->
          Case
            {
              c_tag;
              c_name;
              c_type =
                Typ_primitive { t_generic = Type.unit; t_merge = Merge.unit };
              c_constructor = (fun () -> c_singleton);
              c_witness = Witness.make ();
            })
  end

  include Variant
  include Record

  type nonrec ('a, 'b) field = ('a, 'b) field

  type 'a case_p = 'a case_typeable

  type ('a, 'b, 'c) case = ('a, 'b, 'c) untagged_case
end
