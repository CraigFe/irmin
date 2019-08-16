open Lwt.Infix
open Hiredis
module Helper (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  type 'a t = (string * Client.t) (* Store type: Redis prefix and client *)
  type key = K.t                  (* Key type *)
  type value = V.t                (* Value type *)
  let v prefix config =
    let module C = Irmin.Private.Conf in
    let root = match C.get config C.root with
      | Some root -> root ^ ":" ^ prefix ^ ":"
      | None -> prefix ^ ":"
    in
    Lwt.return (root, Client.connect ~port:6379 "127.0.0.1")
  let mem (prefix, client) key =
      let key = Irmin.Type.to_string K.t key in
      match Client.run client [| "EXISTS"; prefix ^ key |] with
      | Integer 1L -> Lwt.return_true
      | _ -> Lwt.return_false
  let find (prefix, client) key =
      let key = Irmin.Type.to_string K.t key in
      match Client.run client [| "GET"; prefix ^ key |] with
      | String s ->
          (match Irmin.Type.of_string V.t s with
          | Ok s -> Lwt.return_some s
          | _ -> Lwt.return_none)
      | _ -> Lwt.return_none
end
module Content_addressable (K: Irmin.Hash.S) (V: Irmin.Type.S) = struct
  include Helper(K)(V)
  let v = v "obj"
  let add (prefix, client) value =
      let hash = K.hash (fun f -> f @@ Irmin.Type.to_string V.t value) in
      let key = Irmin.Type.to_string K.t hash in
      let value = Irmin.Type.to_string V.t value in
      ignore (Client.run client [| "SET"; prefix ^ key; value |]);
      Lwt.return hash
  let batch (prefix, client) f =
    let _ = Client.run client [| "MULTI" |] in
    f (prefix, client) >|= fun result ->
    let _ = Client.run client [| "EXEC" |] in
    result
end
module Atomic_write (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  module H = Helper(K)(V)
  module W = Irmin.Private.Watch.Make(K)(V)
  type t = { t: [`Write] H.t; w: W.t }  (* Store type *)
  type key = H.key                      (* Key type *)
  type value = H.value                  (* Value type *)
  type watch = W.watch                  (* Watch type *)
  let watches = W.v ()
  let v config =
    H.v "data" config >>= fun t ->
    Lwt.return {t; w = watches }
  let find t = H.find t.t
  let mem t  = H.mem t.t
  let watch_key t key = W.watch_key t.w key
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w
  let list {t = (prefix, client); _} =
      match Client.run client [| "KEYS"; prefix ^ "*" |] with
      | Array arr ->
          Array.map (fun k ->
            Irmin.Type.of_string K.t (Value.to_string k)
          ) arr
          |> Array.to_list
          |> Lwt_list.filter_map_s (function
            | Ok s -> Lwt.return_some s
            | _ -> Lwt.return_none)
      | _ -> Lwt.return []
  let set {t = (prefix, client); w} key value =
      let key' = Irmin.Type.to_string K.t key in
      let value' = Irmin.Type.to_string V.t value in
      match Client.run client [| "SET"; prefix ^ key'; value' |] with
      | Status "OK" -> W.notify w key (Some value)
      | _ -> Lwt.return_unit
  let remove {t = (prefix, client); w} key =
      let key' = Irmin.Type.to_string K.t key in
      ignore (Client.run client [| "DEL"; prefix ^ key' |]);
      W.notify w key None
  let test_and_set t key ~test ~set:set_value =
    (* A helper function to execute a command in a Redis transaction *)
    let txn client args =
      ignore @@ Client.run client [| "MULTI" |];
      ignore @@ Client.run client args;
      Client.run client [| "EXEC" |] <> Nil
    in
    let prefix, client = t.t in
    let key' = Irmin.Type.to_string K.t key in
    (* Start watching the key in question *)
    ignore @@ Client.run client [| "WATCH"; prefix ^ key' |];
    (* Get the existing value *)
    find t key >>= fun v ->
    (* Check it against [test] *)
    if Irmin.Type.(equal (option V.t)) test v then (
      (match set_value with
        | None -> (* Remove the key *)
            if txn client [| "DEL"; prefix ^ key' |] then
              W.notify t.w key None >>= fun () ->
              Lwt.return_true
            else
              Lwt.return_false
        | Some value -> (* Update the key *)
            let value' = Irmin.Type.to_string V.t value in
            if txn client [| "SET"; prefix ^ key'; value' |] then
              W.notify t.w key set_value >>= fun () ->
              Lwt.return_true
            else
              Lwt.return_false
      ) >>= fun ok ->
      Lwt.return ok
    ) else (
      ignore @@ Client.run client [| "UNWATCH"; prefix ^ key' |];
      Lwt.return_false
    )
end
module Make: Irmin.S_MAKER = Irmin.Make(Content_addressable)(Atomic_write)

module KV: Irmin.KV_MAKER = functor (C: Irmin.Contents.S) ->
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
