type ('s, 'a) path = unit

type 'a typ =
  | Typ_record : 'a record -> 'a typ
  | Typ_assoc : 'a Type.t -> 'a typ

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
  f_get : 'record -> 'field Lwt.t;
  f_update : 'record -> 'field -> 'record Lwt.t;
}

and ('record, 'constr, 'lens, 'lens_nil) fields =
  | Fields_nil : ('record, 'constr, 'lens_nil, 'lens_nil) fields
  | Fields_cons :
      ('record, 'field) field * ('record, 'constr, 'lens, 'lens_nil) fields
      -> ( 'record,
           'field -> 'constr,
           ('record, 'field) path * 'lens,
           'lens_ni )
         fields

(** Generic combinators for product types **)
module Record = struct
  module Unwitnessed = struct
    type ('record, 'cons, 'lens, 'lens_nil, 'm) t = {
      name : string;
      cons : 'cons;
      fields : ('record, 'cons, 'lens, 'lens_nil) fields;
    }
  end

  type ('record, 'constr, 'remaining, 'lenses, 'lens_nil, 'm) open_record = {
    open_record :
      'hole. ('record, 'remaining, 'lens_nil, 'hole, 'm) fields ->
      (* Append the two lens lists at the type level *)
      ('record, 'constr, 'lenses, 'hole, 'm) Unwitnessed.t;
  }
  [@@unboxed]

  type nonrec ('a, 'b) field = ('a, 'b) field

  let field fname ftype ?set:fset fget = { fname; ftype; fget; fset }

  let record :
      type r. string -> r -> ('a, r, r, 'lens_nil, 'lens_nil, 'm) open_record =
   fun name cons ->
    let open_record fields = Unwitnessed.{ name; cons; fields } in
    { open_record }

  let app :
      type r c ft rem lens lens_nil m.
      (r, c, ft -> rem, lens, (r, ft, m) O.Mono.lens * lens_nil, m) open_record ->
      (r, ft) field ->
      (r, c, rem, lens, lens_nil, m) open_record =
   fun { open_record = previous } field ->
    let open_record' :
        type hole.
        (r, rem, lens_nil, hole, m) fields ->
        (r, c, lens, hole, m) Unwitnessed.t =
     fun fs -> previous (Fields_cons (field, fs))
    in
    { open_record = open_record' }

  (* Ground lens list with [unit] *)
  let sealr_with_optics :
      type record cons lens m.
      (record, cons, record, lens, unit, m) open_record ->
      record t
      * (m monad ->
        (lens, m, Irmin_optics.Subtyping.lens) Irmin_optics.Optic_list.t) =
   fun { open_record = r } ->
    let Unwitnessed.{ name; cons; fields } = r Fields_nil in
    let rwit = Witness.make () in
    let lenses (monad : m monad) =
      let ( let+ ) x f = monad#fmap f x in
      let rec inner :
          type a l.
          (record, a, l, unit, m) fields ->
          (l, m, Irmin_optics.Subtyping.lens) Irmin_optics.Optic_list.t =
        function
        | Fields_nil -> []
        | Fields_cons ({ fget; fset; _ }, fs) ->
            let fset =
              match fset with
              | Some fset ->
                  fun field_modifier record ->
                    let field = fget record in
                    let+ field' = field_modifier field in
                    fset record field'
              | None -> fun _ _ -> assert false
            in
            let fget = fget >>> monad#return in
            let ml = Irmin_optics.lens monad fget fset in
            ml :: inner fs
      in
      inner fields
    in
    (Record { rwit; rname = name; rfields = Fields (fields, cons) }, lenses)

  let sealr : type a b. (a, b, a, _, _, _) open_record -> a t =
   fun r -> sealr_with_optics r |> fst

  let ( |+ ) = app
end

(** Generic combinators for sum types *)
module Variant = struct
  type t
end
