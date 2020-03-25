open Lwt.Infix
module Sigs = S

module type S = Store_commit_intf.S

module type MAKER = Store_commit_intf.MAKER

module Make : MAKER =
functor
  (P : Sigs.PRIVATE)
  ->
  struct
    module Hash = P.Hash

    type repo = P.Repo.t

    type hash = P.Hash.t

    type t = { r : repo; h : hash; v : P.Commit.value }

    let t r =
      let open Type in
      record "commit" (fun h v -> { r; h; v })
      |+ field "hash" Hash.t (fun t -> t.h)
      |+ field "value" P.Commit.Val.t (fun t -> t.v)
      |> sealr

    let node t = P.Commit.Val.node t.v

    let equal x y = Type.equal Hash.t x.h y.h

    let repo t = t.r

    let hash t = t.h

    let info t = P.Commit.Val.info t.v

    let parents t = P.Commit.Val.parents t.v

    let pp_hash ppf t = Type.pp Hash.t ppf t.h

    let of_hash r h =
      P.Commit.find (P.Repo.commit_t r) h >|= function
      | None -> None
      | Some v -> Some { r; h; v }

    let to_private_commit t = t.v

    let of_private_commit r v =
      let h = P.Commit.Key.hash v in
      { r; h; v }

    let equal_opt x y =
      match (x, y) with
      | None, None -> true
      | Some x, Some y -> equal x y
      | _ -> false
  end
