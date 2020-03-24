module Path = struct
  type nonrec ('s, 'a) t = {
    get : 's -> 'a option Lwt.t;
    update : 's -> 'a -> 's Lwt.t;
  }

  type ('head, 'nil) hlist =
    | [] : ('nil, 'nil) hlist
    | ( :: ) : ('elt * ('head, 'nil) hlist) -> ('elt * 'head, 'nil) hlist
end

type 'a typ =
  | Typ_record : 'a record -> 'a typ
  | Typ_variant : 'a variant -> 'a typ
  | Typ_assoc : 'a Type.t -> 'a typ

(* Records *)
and 'record record =
  | Record : {
      r_witness : 'record Irmin_type.Witness.t;
      r_name : string;
      r_fields : ('record, 'cons, _, _) fields;
      r_constructor : 'cons;
    }
      -> 'a record

and ('record, 'field) field = {
  f_name : string;
  f_type : 'field Type.t;
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
  v_witness : 'variant Irmin_type.Witness.t;
  v_name : string;
  v_cases : 'variant case_exists array;
  v_get : 'variant -> 'variant case_typeable;
}

and ('variant, 'arg) case = {
  c_witness : 'arg Irmin_type.Witness.t;
  c_tag : int;
  c_name : string;
  c_arg_type : 'arg Type.t;
  c_constructor : 'arg -> 'variant;
}

and 'variant case_exists =
  | Case_exists : ('variant, 'arg) case -> 'variant case_exists

and 'variant case_typeable =
  | Case_typable : ('variant, 'case) case * 'case -> 'variant case_typeable

and ('variant, 'pat, 'pat_nil, 'prisms, 'prism_nil) cases =
  | Cases_nil : ('variant, 'pat_nil, 'pat_nil, 'prism_nil, 'prism_nil) cases
  | Cases_cons :
      ('variant, 'case) case
      * ('variant, 'remaining, 'pat_nil, 'prisms, 'prism_nil) cases
      -> ( 'variant,
           'constr -> 'remaining,
           'pat_nil,
           ('variant, 'case) Path.t * 'prisms,
           'prism_nil )
         cases

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

  let field f_name f_type ~update:f_update f_get =
    { f_name; f_type; f_get; f_update }

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
    let r_witness = Irmin_type.Witness.make () in
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
  type ('v, 'pat, 'rem, 'rem_nil, 'prism, 'prism_nil, 'm) open_variant = {
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
    | Cases_cons (case, cs) ->
        add_cases_to_destructor (p (fun v -> Case_typeable (case, v))) cs

  let variant : string -> 'p -> ('v, 'p, 'r, 'r, 'opt, 'opt, 'm) open_variant =
   fun n p ->
    let open_variant cs = (n, add_cases_to_destructor p cs, cs) in
    { open_variant; next_tag = 0 }

  let ( |~ ) :
      type v c constr pat rem rem_nil opt opt_nil m.
      ( v,
        pat,
        rem,
        constr -> rem_nil,
        opt,
        (v, c) Path.t * opt_nil,
        m )
      open_variant ->
      (v, c) case ->
      (v, pat, rem, rem_nil, opt, opt_nil, m) open_variant =
   fun { open_variant = previous; next_tag } case ->
    let open_variant' cs = previous (Cases_cons (case next_tag, cs)) in
    { open_variant = open_variant'; next_tag = next_tag + 1 }

  let array_of_case_list cases =
    let rec inner :
        type variant pat pat_nil pri pri_nil m.
        (variant, pat, pat_nil, pri, pri_nil, m) cases -> variant a_case list =
      function
      | Cases_nil -> []
      | Cases_cons (C0 c, cs) -> CP0 c :: inner cs
      | Cases_cons (C1 c, cs) -> CP1 c :: inner cs
    in
    inner cases |> Array.of_list

  let preview : type v c. (v -> v case_v) -> c Witness.t -> int -> v -> c option
      =
   fun vget type_expected tag_expected v ->
    vget v |> function
    | CV1 ({ ctag1 = tag_actual; c1_witness = type_actual; _ }, elt) ->
        if tag_actual = tag_expected then
          Witness.cast type_actual type_expected elt
        else None
    | CV0 _ -> None

  let seal :
      type variant pat prisms m.
      ( variant,
        pat,
        pat,
        variant -> variant case_v,
        prisms,
        unit,
        m )
      open_variant ->
      variant t * (m monad -> (prisms, m) Prism.t_list) =
   fun { open_variant = v; _ } ->
    let vname, vget, cases = v Cases_nil in
    let vget v = vget v in
    let vwit = Witness.make () in
    let vcases = array_of_case_list cases in
    let prisms (monad : m monad) =
      let rec inner :
          type p a b. (variant, a, b, p, unit, m) cases -> (p, m) Prism.t_list =
        function
        | Cases_nil -> []
        | Cases_cons (C0 { c0; ctag0 = tag_expected; _ }, cs) ->
            let review () = monad#return c0 in
            let preview v =
              vget v
              |> (function
                   | CV0 { ctag0 = tag_actual; _ } ->
                       if tag_actual = tag_expected then Some () else None
                   | CV1 _ -> None)
              |> monad#return
            in
            Prism.v monad review preview :: inner cs
        | Cases_cons
            (C1 { c1; ctag1 = tag_expected; c1_witness = type_expected; _ }, cs)
          ->
            let review b = monad#return (c1 b) in
            let preview s =
              preview vget type_expected tag_expected s |> monad#return
            in
            Prism.v monad review preview :: inner cs
      in
      inner cases
    in
    (Variant { vwit; vname; vcases; vget }, prisms)

  type 'a case_p = 'a case_v

  let case1 c_name c_arg_type c_constructor c_tag =
    {
      c_witness = Irmin_type.Witness.make ();
      c_tag;
      c_name;
      c_arg_type;
      c_constructor;
    }

  let case0 c_name c_singleton c_tag =
    case1 c_name Type.unit (fun () -> c_singleton) c_tag
end
