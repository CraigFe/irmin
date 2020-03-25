open Lwt.Infix

module type S = Repo_intf.S

module type MAKER = Repo_intf.MAKER

module Make (Log : Logs.LOG) (P : S.PRIVATE) = struct
  module Branch_store = P.Branch
  module H = Commit.History (P.Commit)
  module Commit = Store_commit.Make (P)

  type commit = Commit.t

  type branch = Branch_store.key

  type slice = P.Slice.t

  module OCamlGraph = Graph
  module Graph = Node.Graph (P.Node)
  module KGraph =
    Object_graph.Make (P.Contents.Key) (P.Node.Metadata) (P.Node.Key)
      (P.Commit.Key)
      (Branch_store.Key)

  type t = P.Repo.t

  let v = P.Repo.v

  let close = P.Repo.close

  let graph_t t = P.Repo.node_t t

  let history_t t = P.Repo.commit_t t

  let commit_t t = P.Repo.commit_t t

  let node_t t = P.Repo.node_t t

  let contents_t t = P.Repo.contents_t t

  let branch_t t = P.Repo.branch_t t

  let branches t = P.Branch.list (branch_t t)

  let heads repo =
    let t = branch_t repo in
    Branch_store.list t >>= fun bs ->
    Lwt_list.fold_left_s
      (fun acc r ->
        Branch_store.find t r >>= function
        | None -> Lwt.return acc
        | Some h -> (
            Commit.of_hash repo h >|= function
            | None -> acc
            | Some h -> h :: acc ))
      [] bs

  let export ?(full = true) ?depth ?(min = []) ?(max = `Head) t =
    Log.debug (fun f ->
        f "export depth=%s full=%b min=%d max=%s"
          (match depth with None -> "<none>" | Some d -> string_of_int d)
          full (List.length min)
          ( match max with
          | `Head -> "heads"
          | `Max m -> string_of_int (List.length m) ));
    (match max with `Head -> heads t | `Max m -> Lwt.return m) >>= fun max ->
    P.Slice.empty () >>= fun slice ->
    let max = List.map (fun x -> `Commit (Commit.hash x)) max in
    let min = List.map (fun x -> `Commit (Commit.hash x)) min in
    let pred = function
      | `Commit k ->
          H.parents (history_t t) k >|= fun parents ->
          List.map (fun x -> `Commit x) parents
      | _ -> Lwt.return_nil
    in
    KGraph.closure ?depth ~pred ~min ~max () >>= fun g ->
    let keys =
      List.fold_left
        (fun acc -> function `Commit c -> c :: acc | _ -> acc)
        [] (KGraph.vertex g)
    in
    let root_nodes = ref [] in
    Lwt_list.iter_p
      (fun k ->
        P.Commit.find (commit_t t) k >>= function
        | None -> Lwt.return_unit
        | Some c ->
            root_nodes := P.Commit.Val.node c :: !root_nodes;
            P.Slice.add slice (`Commit (k, c)))
      keys
    >>= fun () ->
    if not full then Lwt.return slice
    else
      (* XXX: we can compute a [min] if needed *)
      Graph.closure (graph_t t) ~min:[] ~max:!root_nodes >>= fun nodes ->
      let module KSet = Set.Make (struct
        type t = P.Contents.key

        let compare = Type.compare P.Contents.Key.t
      end) in
      let contents = ref KSet.empty in
      Lwt_list.iter_p
        (fun k ->
          P.Node.find (node_t t) k >>= function
          | None -> Lwt.return_unit
          | Some v ->
              List.iter
                (function
                  | _, `Contents (c, _) -> contents := KSet.add c !contents
                  | _ -> ())
                (P.Node.Val.list v);
              P.Slice.add slice (`Node (k, v)))
        nodes
      >>= fun () ->
      Lwt_list.iter_p
        (fun k ->
          P.Contents.find (contents_t t) k >>= function
          | None -> Lwt.return_unit
          | Some m -> P.Slice.add slice (`Contents (k, m)))
        (KSet.elements !contents)
      >|= fun () -> slice

  exception Import_error of string

  let import_error fmt = Fmt.kstrf (fun x -> Lwt.fail (Import_error x)) fmt

  let import t s =
    let aux name add dk (k, v) =
      add v >>= fun k' ->
      if not (Type.equal dk k k') then
        import_error "%s import error: expected %a, got %a" name
          Type.(pp dk)
          k
          Type.(pp dk)
          k'
      else Lwt.return_unit
    in
    let contents = ref [] in
    let nodes = ref [] in
    let commits = ref [] in
    P.Slice.iter s (function
      | `Contents c ->
          contents := c :: !contents;
          Lwt.return_unit
      | `Node n ->
          nodes := n :: !nodes;
          Lwt.return_unit
      | `Commit c ->
          commits := c :: !commits;
          Lwt.return_unit)
    >>= fun () ->
    P.Repo.batch t @@ fun contents_t node_t commit_t ->
    Lwt.catch
      (fun () ->
        Lwt_list.iter_p
          (aux "Contents" (P.Contents.add contents_t) P.Contents.Key.t)
          !contents
        >>= fun () ->
        Lwt_list.iter_p (aux "Node" (P.Node.add node_t) P.Node.Key.t) !nodes
        >>= fun () ->
        Lwt_list.iter_p
          (aux "Commit" (P.Commit.add commit_t) P.Commit.Key.t)
          !commits
        >|= fun () -> Ok ())
      (function
        | Import_error e -> Lwt.return_error (`Msg e)
        | e -> Fmt.kstrf Lwt.fail_invalid_arg "impot error: %a" Fmt.exn e)
end
