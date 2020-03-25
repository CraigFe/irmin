module Witness = Irmin_type.Witness

module Path = struct
  type nonrec ('s, 'a) t = {
    get : 's -> 'a option Lwt.t;
    update : 's -> 'a -> 's Lwt.t;
  }

  let ( / ) outer inner =
    let open Lwt.Infix in
    let get s =
      outer.get s >>= function
      | Some mid -> inner.get mid
      | None -> Lwt.return None
    in
    let update s a =
      outer.get s >>= function
      | None -> Lwt.return s
      | Some mid -> inner.update mid a >>= fun mid -> outer.update s mid
    in
    { get; update }

  let id =
    let get s = Lwt.return (Some s) in
    let update _ s = Lwt.return s in
    { get; update }

  type ('head, 'nil) hlist =
    | [] : ('nil, 'nil) hlist
    | ( :: ) : ('elt * ('head, 'nil) hlist) -> ('elt * 'head, 'nil) hlist
end

type 'a typ =
  | Typ_record : 'a record -> 'a typ
      (** Product of zero-or-more constituent trees *)
  | Typ_variant : 'a variant -> 'a typ
      (** Sum of zero-or-more constituent trees *)
  | Typ_assoc : 'a typ -> 'a typ
      (** An association list from strings to trees of the given type *)
  | Type_tree : 'a typ -> 'a typ
  | Typ_primitive : 'a Type.t -> 'a typ
      (** Stores exactly one value of a given type *)

(* Records *)
and 'record record =
  | Record : {
      r_witness : 'record Witness.t;
      r_name : string;
      r_fields : ('record, 'cons, _, _) fields;
      r_constructor : 'cons;
    }
      -> 'a record

and ('record, 'field) field = {
  f_name : string;
  f_type : 'field typ;
  f_get : 'record -> 'field;
  f_update : 'record -> 'field -> 'record;
}

and ('record, 'constr, 'lens, 'lens_nil) fields =
  | Fields_nil : ('record, 'constr, 'lens_nil, 'lens_nil) fields
  | Fields_cons :
      ('record, 'field) field * ('record, 'constr, 'lens, 'lens_nil) fields
      -> ( 'record,
           'field -> 'constr,
           ('record, 'field) Path.t * 'lens,
           'lens_nil )
         fields

(* Variants *)
and 'variant variant = {
  v_witness : 'variant Witness.t;
  v_name : string;
  v_cases : 'variant case_exists array;
  v_get : 'variant -> 'variant case_typeable;
}

and ('variant, 'arg, 'constr) case =
  | Case : {
      c_tag : int;
      c_name : string;
      c_type : 'arg typ;
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

and ('variant, 'pat, 'pat_nil, 'prisms, 'prism_nil) cases =
  | Cases_nil : ('variant, 'pat_nil, 'pat_nil, 'prism_nil, 'prism_nil) cases
  | Cases_cons :
      ('variant, 'case, 'constr) case
      * ('variant, 'remaining, 'pat_nil, 'prisms, 'prism_nil) cases
      -> ( 'variant,
           'constr -> 'remaining,
           'pat_nil,
           ('variant, 'case) Path.t * 'prisms,
           'prism_nil )
         cases

type 'a t = 'a typ

(** Generic combinators for product types **)
module Record = struct
  module Unwitnessed = struct
    type ('record, 'cons, 'lens, 'lens_nil) t = {
      name : string;
      cons : 'cons;
      fields : ('record, 'cons, 'lens, 'lens_nil) fields;
    }
  end

  type ('record, 'constr, 'remaining, 'lens, 'lens_nil) open_record = {
    open_record :
      'hole. ('record, 'remaining, 'lens_nil, 'hole) fields ->
      (* Append the two lens lists at the type level *)
      ('record, 'constr, 'lens, 'hole) Unwitnessed.t;
  }
  [@@unboxed]

  type nonrec ('a, 'b) field = ('a, 'b) field

  let field f_name f_type f_update f_get = { f_name; f_type; f_get; f_update }

  let record :
      type r. string -> r -> ('a, r, r, 'lens_nil, 'lens_nil) open_record =
   fun name cons ->
    let open_record fields = Unwitnessed.{ name; cons; fields } in
    { open_record }

  let ( |+ ) :
      type r c ft rem lens lens_nil.
      (r, c, ft -> rem, lens, (r, ft) Path.t * lens_nil) open_record ->
      (r, ft) field ->
      (r, c, rem, lens, lens_nil) open_record =
   fun { open_record = previous } field ->
    let open_record' :
        type hole.
        (r, rem, lens_nil, hole) fields -> (r, c, lens, hole) Unwitnessed.t =
     fun fs -> previous (Fields_cons (field, fs))
    in
    { open_record = open_record' }

  (* Ground lens list with [unit] *)
  let seal :
      type record_t cons lens.
      (record_t, cons, record_t, lens, unit) open_record ->
      record_t record * (lens, unit) Path.hlist =
   fun { open_record = r } ->
    let Unwitnessed.{ name; cons; fields } = r Fields_nil in
    let r_witness = Witness.make () in
    let lenses =
      let rec inner :
          type a l. (record_t, a, l, unit) fields -> (l, unit) Path.hlist =
        function
        | Fields_nil -> []
        | Fields_cons ({ f_get; f_update; _ }, fs) ->
            let get s = Lwt.return (Some (f_get s)) in
            let update s a = Lwt.return (f_update s a) in
            let ml = Path.{ get; update } in
            ml :: inner fs
      in
      inner fields
    in
    ( Record
        { r_witness; r_name = name; r_fields = fields; r_constructor = cons },
      lenses )
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

  let variant : string -> 'p -> ('v, 'p, 'r, 'r, 'opt, 'opt) open_variant =
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
        (v, c) Path.t * opt_nil )
      open_variant ->
      (v, c, constr) untagged_case ->
      (v, pat, rem, rem_nil, opt, opt_nil) open_variant =
   fun { open_variant = previous; next_tag } (Untagged case) ->
    let open_variant' cs = previous (Cases_cons (case next_tag, cs)) in
    { open_variant = open_variant'; next_tag = next_tag + 1 }

  let array_of_case_list cases =
    let rec inner :
        type variant pat pat_nil pri pri_nil.
        (variant, pat, pat_nil, pri, pri_nil) cases -> variant case_exists list
        = function
      | Cases_nil -> []
      | Cases_cons (c, cs) -> Case_exists c :: inner cs
    in
    inner cases |> Array.of_list

  let preview :
      type v c. (v -> v case_typeable) -> c Witness.t -> int -> v -> c option =
   fun vget type_expected tag_expected v ->
    let (Case_typeable
          (Case { c_tag = tag_actual; c_witness = type_actual; _ }, elt)) =
      vget v
    in
    if tag_actual = tag_expected then Witness.cast type_actual type_expected elt
    else None

  let paths_of_cases (type variant) (v_get : variant -> variant case_typeable) =
    let rec inner :
        type a b paths.
        (variant, a, b, paths, unit) cases -> (paths, unit) Path.hlist =
      function
      | Cases_nil -> []
      | Cases_cons
          ( Case
              {
                c_constructor;
                c_tag = tag_expected;
                c_witness = type_expected;
                _;
              },
            cs ) ->
          let update s a =
            let (Case_typeable (Case { c_tag = tag_actual; _ }, _)) = v_get s in
            (if Int.equal tag_actual tag_expected then c_constructor a else s)
            |> Lwt.return
          in
          let get s =
            preview v_get type_expected tag_expected s |> Lwt.return
          in
          let path = Path.{ get; update } in
          path :: inner cs
    in
    inner

  let seal :
      type variant pat prisms.
      ( variant,
        pat,
        pat,
        variant -> variant case_typeable,
        prisms,
        unit )
      open_variant ->
      variant typ * (prisms, unit) Path.hlist =
   fun { open_variant = v; _ } ->
    let v_name, v_get, cases = v Cases_nil in
    let v_witness = Witness.make () in
    let v_cases = array_of_case_list cases in
    let paths = paths_of_cases v_get cases in
    (Typ_variant { v_witness; v_name; v_cases; v_get }, paths)

  let case1 c_name c_type c_constructor =
    Untagged
      (fun c_tag ->
        Case
          { c_tag; c_name; c_type; c_constructor; c_witness = Witness.make () })

  let case0 c_name c_singleton =
    Untagged
      (fun c_tag ->
        Case
          {
            c_tag;
            c_name;
            c_type = Typ_primitive Type.unit;
            c_constructor = (fun () -> c_singleton);
            c_witness = Witness.make ();
          })
end
