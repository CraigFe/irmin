include Dict.Make (IO.Unix)

(* Add IO caching around Dict.v *)
let (`Staged v) =
  let v_no_cache ~fresh ~readonly = v ~fresh ~readonly in
  IO.with_cache ~clear
    ~v:(fun capacity ~fresh ~shared:_ ~readonly ->
      v_no_cache ~fresh ~readonly ~capacity)
    "store.dict"

let v ?fresh ?readonly ?(capacity = 100_000) root =
  v capacity ?fresh ?readonly root
